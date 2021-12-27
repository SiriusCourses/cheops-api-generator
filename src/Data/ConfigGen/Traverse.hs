{-# LANGUAGE RecordWildCards #-}

module Data.ConfigGen.Traverse
    ( build
    ) where

import Control.Monad.Except       (Except, MonadError (..), runExcept)
import Control.Monad.Reader       (ReaderT (..), asks, withReaderT)
import Control.Monad.State.Strict (MonadState (..), StateT (..), modify)

import           Data.Bifunctor (bimap)
import           Data.Coerce    (coerce)
import           Data.Maybe     (mapMaybe)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Data.String    (fromString)
import           Util           (singleton)

import           Data.ConfigGen.Parsing        (ParserResult (ParserResult))
import qualified Data.ConfigGen.Traverse.Hylo  as Hylo
import qualified Data.ConfigGen.Traverse.Utils as U
import           Data.ConfigGen.TypeRep        (ModuleName, ModuleParts (..), TypeRep)
import qualified Data.ConfigGen.TypeRep        as TR

import GHC.SourceGen (ConDecl', HsDecl', HsModule', ImportDecl', Var (var), data', field,
                      import', module', newtype', prefixCon, qualified', recordCon, strict,
                      type', (@@))

import           Data.Foldable   (toList)
import           Data.Map        (Map)
import qualified Data.Map        as Map

data Dep a
    = Built
    | ToBuild a
    deriving (Show, Eq, Ord)

newtype GeneratorState =
    GeneratorState
        { includes :: Map FilePath (Dep ModuleParts)
        }
    deriving newtype (Semigroup, Monoid, Show)

type Ctx a = ReaderT U.ModulePrefix (StateT GeneratorState (Except String)) a

data NodeF a s
    = Local a (Map ModuleName s)
    | Leaf a

getPayload :: NodeF a s -> a
getPayload (Local p _) = p
getPayload (Leaf p)    = p

data Payload =
    Payload
        { title        :: Maybe U.Title
        , externalDeps :: Set FilePath
        , typeRep      :: TypeRep
        }

instance Functor (NodeF a) where
    fmap _ (Leaf x)       = Leaf x
    fmap fab (Local x km) = Local x $ fmap fab km

breakDown :: Hylo.Coalgebra (NodeF Payload) ModuleParts
breakDown (ModuleParts _jsTitle _externalDeps _localDeps _declaration)
    | null _localDeps = Leaf payload
    | otherwise = Local payload _localDeps
  where
    payload = Payload _jsTitle _externalDeps _declaration

buildUp :: Hylo.Algebra (NodeF Payload) (Ctx (Map FilePath HsModule'))
buildUp node = do
    let p@(Payload _ externalDeps _) = getPayload node
    localDeps <-
        case node of
            Local _ km ->
                fmap (Map.foldl' (<>) mempty) . sequence $
                Map.mapWithKey (withReaderT . U.updatePrefix) km
            Leaf _ -> return mempty
    extDeps <- mconcat <$> traverse buildExternalDep (Set.toList externalDeps)
    newModule <- asks $ buildModule p
    path <- asks U.prefixToPath
    return $ Map.singleton path newModule <> extDeps <> localDeps

buildModule :: Payload -> U.ModulePrefix -> HsModule'
buildModule Payload {..} prefix =
    let extImports =
            qualified' . import' . fromString . U.prefixToModuleName . U.pathToPrefix <$>
            Set.toList externalDeps
        exports = Nothing
        defaultImports =
            qualified' . import' . fromString <$>
            ["GHC.Types", "GHC.Int", "Data.Text", "Data.Vector"]
     in module'
            (Just . fromString $ U.prefixToModuleName prefix)
            exports
            (extImports <> gatherLocalImports typeRep <> defaultImports)
            [buildTypeDecl typeRep]
  where
    typename :: TR.TypeName
    typename = U.prefixToTypeName prefix title
    gatherLocalImports :: TR.TypeRep -> [ImportDecl']
    gatherLocalImports tr
        | (TR.ProdType map') <- tr = gather map'
        | (TR.SumType map') <- tr = gather map'
      where
        gather :: Map U.FieldName TR.TypeRef -> [ImportDecl']
        gather = mapMaybe snd . Map.toList . Map.map go
          where
            go :: TR.TypeRef -> Maybe ImportDecl'
            go tr' = qualified' . import' . fromString <$> U.referenceToModuleName prefix tr'
    gatherLocalImports (TR.ArrayType tr') =
        toList $ qualified' . import' . fromString <$> U.referenceToModuleName prefix tr'
    gatherLocalImports (TR.NewType tr') =
        toList $ qualified' . import' . fromString <$> U.referenceToModuleName prefix tr'
    gatherLocalImports (TR.Ref nlr) =
        toList $ qualified' . import' . fromString <$> U.nonLocalReferenceToModuleName nlr
    buildTypeDecl :: TR.TypeRep -> HsDecl'
    buildTypeDecl (TR.ProdType map') = data' (fromString typename) [] [buildProdCon map'] []
      where
        buildProdCon :: Map TR.FieldName TR.TypeRef -> ConDecl'
        buildProdCon km =
            recordCon (fromString typename) $
            bimap
                (fromString . U.changeReservedNames)
                (strict . field . var . fromString . U.referenceToQualTypeName prefix) <$>
            Map.toList km
    buildTypeDecl (TR.SumType map') = data' (fromString typename) [] (buildSumCon's map') []
      where
        buildSumCon's :: Map TR.FieldName TR.TypeRef -> [ConDecl']
        buildSumCon's km =
            let constructorFromPair k v =
                    prefixCon (fromString . U.changeReservedNames $ k) $
                    singleton . (strict . field . var . fromString . U.referenceToTypeName) $ v
             in uncurry constructorFromPair <$> Map.toList km
    buildTypeDecl (TR.ArrayType tr') = type' (fromString typename) [] $ listType @@ listItem
      where
        listType = var "Data.Vector.Vector"
        listItem = var . fromString $ U.referenceToQualTypeName prefix tr'
    buildTypeDecl (TR.NewType tr') =
        let newtypename = fromString typename
            constructorName = fromString typename
            internalType = field . var . fromString $ U.referenceToQualTypeName prefix tr'
            getter = fromString $ U.getterName typename
         in newtype' newtypename [] (recordCon constructorName [(getter, internalType)]) []
    buildTypeDecl (TR.Ref nlr) = type' (fromString typename) [] symtype
      where
        symtype = var . fromString $ U.nonLocalReferenceToQualTypeName nlr

buildExternalDep :: FilePath -> Ctx (Map FilePath HsModule')
buildExternalDep path = do
    state' <- get
    let moduleToBuild = Map.lookup (fromString path) (includes state')
    case moduleToBuild of
        Nothing ->
            throwError $
            "While buildig external dependency" ++
            show path ++ " did not find it among includes " ++ show state'
        Just Built -> return mempty
        Just (ToBuild yetTobuild) -> do
            let modulePrefix = U.pathToPrefix path
            let (newState :: GeneratorState) =
                    coerce . Map.delete (fromString path) $
                    (coerce state' :: Map FilePath (Dep ModuleParts))
            put newState
            builtModules <- withReaderT (const modulePrefix) $ modulePartsToModules yetTobuild
            modify $ \incs -> GeneratorState $ Map.insert (fromString path) Built $ coerce incs
            return builtModules

buildOrphanDeps :: Ctx (Map FilePath HsModule')
buildOrphanDeps = do
    nms <- fmap fst . filter f . Map.toList . includes <$> get
    mconcat <$> traverse buildExternalDep nms
  where
    f (_, ToBuild _) = True
    f (_, Built)     = False

modulePartsToModules :: ModuleParts -> Ctx (Map FilePath HsModule')
modulePartsToModules = Hylo.hylo breakDown buildUp

build :: ParserResult -> Either String (Map FilePath HsModule')
build (ParserResult _ deps) = do
    (km, _) <-
        runExcept $
        runStateT (runReaderT buildOrphanDeps mempty) $
        GeneratorState . Map.fromList $ Data.Bifunctor.bimap fromString ToBuild <$> deps
    return km

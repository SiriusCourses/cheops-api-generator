{-# LANGUAGE RecordWildCards #-}

module Data.ConfigGen.Traverse
    ( build
    ) where

import Control.Monad.Except       (Except, MonadError (..), runExcept)
import Control.Monad.Reader       (ReaderT (..), asks, withReaderT)
import Control.Monad.State.Strict (MonadState (..), StateT (..), modify)

import           Control.Monad  ((<=<))
import           Data.Bifunctor (bimap)
import           Data.Coerce    (coerce)
import           Data.Foldable  (toList)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Maybe     (catMaybes, mapMaybe)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Data.String    (fromString)

import qualified Data.ConfigGen.ModuleParts    as MP
import           Data.ConfigGen.Parsing        (ParserResult (ParserResult))
import qualified Data.ConfigGen.Traverse.Hylo  as Hylo
import qualified Data.ConfigGen.Traverse.Utils as U
import           Data.ConfigGen.TypeRep        (ModuleName, TypeRep)
import qualified Data.ConfigGen.TypeRep        as TR

import GHC.SourceGen (ConDecl', Field, HsDecl', HsModule', HsType', ImportDecl', Var (var),
                      data', field, import', module', newtype', prefixCon, qualified',
                      recordCon, strict, type', (@@))

data Dep a
    = Built
    | ToBuild a
    deriving (Show, Eq, Ord)

newtype GeneratorState =
    GeneratorState
        { includes :: Map FilePath (Dep MP.ModuleParts)
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

breakDown :: Hylo.Coalgebra (NodeF Payload) MP.ModuleParts
breakDown (MP.ModuleParts _jsTitle _externalDeps _localDeps _declaration)
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
        defaultImports = qualified' . import' . fromString <$> U.defaultImportNames
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
        | TR.AllOf <- tr = mempty
        | TR.AnyOf <- tr = mempty
    gatherLocalImports tr
        | (TR.ProdType map') <- tr = gatherProd map'
        | (TR.SumType map') <- tr = gatherSum map'
      where
        gatherSum :: Map U.FieldName TR.SumConstr -> [ImportDecl']
        gatherSum = (catMaybes . snd) <=< Map.toList . fmap TR.unSumConstr . Map.map (fmap go)
          where
            go :: TR.Field -> Maybe ImportDecl'
            go (TR.Field _ tr') =
                qualified' . import' . fromString <$> U.referenceToModuleName prefix tr'
        gatherProd :: Map U.FieldName TR.Field -> [ImportDecl']
        gatherProd = mapMaybe snd . Map.toList . Map.map go
          where
            go :: TR.Field -> Maybe ImportDecl'
            go (TR.Field _ tr') =
                qualified' . import' . fromString <$> U.referenceToModuleName prefix tr'
    gatherLocalImports (TR.ArrayType tr') =
        toList $ qualified' . import' . fromString <$> U.referenceToModuleName prefix tr'
    gatherLocalImports (TR.NewType tr') =
        toList $ qualified' . import' . fromString <$> U.referenceToModuleName prefix tr'
    gatherLocalImports (TR.Ref nlr) =
        toList $ qualified' . import' . fromString <$> U.nonLocalReferenceToModuleName nlr
    transformField :: TR.Field -> Field
    transformField (TR.Field req tr) =
        let tt = var . fromString $ U.referenceToQualTypeName prefix tr
         in strict . field $ maybeWrapper req tt
      where
        maybeWrapper :: Bool -> HsType' -> HsType'
        maybeWrapper True  = (var "Maybe" @@)
        maybeWrapper False = id
    buildTypeDecl :: TR.TypeRep -> HsDecl'
    buildTypeDecl TR.AnyOf =
        data' (fromString typename) [] [prefixCon "AnyOf" []] U.defaultDerivingCause
    buildTypeDecl TR.AllOf =
        data' (fromString typename) [] [prefixCon "AllOf" []] U.defaultDerivingCause
    buildTypeDecl (TR.ProdType map') =
        data' (fromString typename) [] [buildProdCon map'] U.defaultDerivingCause
      where
        buildProdCon :: Map TR.FieldName TR.Field -> ConDecl'
        buildProdCon km =
            recordCon (fromString typename) $
            bimap (fromString . U.changeReservedNames) transformField <$> Map.toList km
    buildTypeDecl (TR.SumType map') =
        data' (fromString typename) [] (buildSumCon's map') U.defaultDerivingCause
      where
        buildSumCon's :: Map TR.FieldName TR.SumConstr -> [ConDecl']
        buildSumCon's km =
            let constructorFromPair :: String -> TR.SumConstr -> ConDecl'
                constructorFromPair k v =
                    prefixCon (fromString . U.fieldNameToSumCon . U.changeReservedNames $ k) $
                    TR.unSumConstr $ transformField <$> v
             in uncurry constructorFromPair <$> Map.toList km
    buildTypeDecl (TR.ArrayType tr') = type' (fromString typename) [] $ listType @@ listItem
      where
        listType = var "Data.Vector.Vector"
        listItem = var . fromString $ U.referenceToQualTypeName prefix tr'
    buildTypeDecl (TR.NewType tr') =
        newtype'
            newtypename
            []
            (recordCon constructorName [(getter, internalType)])
            U.defaultDerivingCause
      where
        newtypename = fromString typename
        constructorName = fromString typename
        internalType = field . var . fromString $ U.referenceToQualTypeName prefix tr'
        getter = fromString $ U.getterName typename
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
                    (coerce state' :: Map FilePath (Dep MP.ModuleParts))
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

modulePartsToModules :: MP.ModuleParts -> Ctx (Map FilePath HsModule')
modulePartsToModules = Hylo.hylo breakDown buildUp

build :: ParserResult -> Either String (Map FilePath HsModule')
build (ParserResult _ deps) = do
    (km, _) <-
        runExcept $
        runStateT (runReaderT buildOrphanDeps mempty) $
        GeneratorState . Map.fromList $ Data.Bifunctor.bimap fromString ToBuild <$> deps
    return km

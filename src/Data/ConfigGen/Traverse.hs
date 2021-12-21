{-# LANGUAGE RecordWildCards #-}

module Data.ConfigGen.Traverse
    ( build
    ) where

import Control.Monad.Except       (Except, MonadError (..), runExcept)
import Control.Monad.Reader       (MonadReader (..), ReaderT (..), asks, local, withReaderT)
import Control.Monad.State.Strict (MonadState (..), StateT (..), modify)

import           Data.Bifunctor  (bimap)
import qualified Data.Bifunctor
import           Data.Coerce     (coerce)
import           Data.List       (foldl', intersperse)
import           Data.Maybe      (catMaybes)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.String     (fromString)
import           System.FilePath (dropExtension, (<.>), (</>))
import           Util            (capitalise, singleton, split)

import           Data.ConfigGen.Parsing       (ParserResult (ParserResult), Title)
import qualified Data.ConfigGen.Traverse.Hylo as Hylo
import           Data.ConfigGen.TypeRep       (ModuleName, ModuleParts (ModuleParts), TypeRep)
import qualified Data.ConfigGen.TypeRep       as TR

import Data.Function ((&))
import GHC.SourceGen (App ((@@)), ConDecl', Field, HsDecl', HsModule', ImportDecl', Var (var),
                      data', field, import', module', newtype', prefixCon, qualified',
                      recordCon, strict, type')

import           Data.Map    (Map)
import qualified Data.Map    as Map


type ModulePrefix = [String]

type PackageName = String

type DeclName = String

data Dep a
    = Built
    | ToBuild a
    deriving (Show, Eq, Ord)

newtype GeneratorState =
    GeneratorState
        { includes :: Map FilePath (Dep ModuleParts)
        }
    deriving newtype (Semigroup, Monoid, Show)

type Ctx a = ReaderT ModulePrefix (StateT GeneratorState (Except String)) a

data NodeF a s
    = Local a (Map ModuleName s)
    | Leaf a

getPayload :: NodeF a s -> a
getPayload (Local p _) = p
getPayload (Leaf p)    = p

data Payload =
    Payload
        { title        :: Maybe String
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
    let p@(Payload title externalDeps _) = getPayload node
    localDeps <-
        case node of
            Local _ km -> do
                built <-
                    fmap (Map.foldl' (<>) mempty) . sequence $
                    Map.mapWithKey (withReaderT . updatePrefix title) $ km
                return built
            Leaf _ -> return mempty
    extDeps <- mconcat <$> (traverse buildExternalDep $ (Set.toList externalDeps))
    path <- asks $ extractPath title
    (fullModuleName, declName) <- asks $ extractFullPackageName title
    let newModule = buildModule fullModuleName declName p
    return $ Map.singleton path newModule <> extDeps <> localDeps

updatePrefix :: Maybe Title -> TR.FieldName -> ModulePrefix -> ModulePrefix
updatePrefix (Just title) fieldName [] = [capitalise $ fieldName, capitalise fieldName]
updatePrefix _ fieldName prefix        = (capitalise $ fieldName) : prefix

extractPath :: Maybe Title -> ModulePrefix -> FilePath
extractPath _ prefix = (foldl' (</>) mempty . reverse $ prefix) <.> "hs"
-- extractPath (Just title) prefix =
--     (foldl' (</>) mempty . reverse . drop 1 $ prefix) </> capitalise title <.> "hs"

extractFullPackageName :: Maybe Title -> ModulePrefix -> (PackageName, DeclName)
extractFullPackageName _ prefix =
    (mconcat . intersperse "." . reverse $ prefix, takeHeadOrFail prefix)
  where
    takeHeadOrFail (x:_) = x
    takeHeadOrFail _     = "Unnamed" -- error "Prefix is emtpy, no Name to give"
-- extractFullPackageName (Just title) prefix =
--     ( mconcat . intersperse "." $ reverse (capitalise title : drop 1 prefix)
--     , capitalise $ title)

pathToModuleName :: String -> String
pathToModuleName s = (mconcat . intersperse "." $ capitalise <$> split '/' s) & dropExtension

buildModule :: PackageName -> DeclName -> Payload -> HsModule'
buildModule pkgName declName Payload {..} =
    let exports = Nothing
        extImports =
            qualified' . import' . fromString . pathToModuleName <$> Set.toList externalDeps
        (decls, locImports) = Data.Bifunctor.first singleton $ go typeRep
        defaultImports =
            qualified' . import' . fromString <$>
            ["GHC.Types", "GHC.Int", "Data.Text", "Data.Vector"]
     in module'
            (Just . fromString $ pkgName)
            exports
            (extImports <> locImports <> defaultImports)
            decls
  where
    typeRefToQualTypeName (TR.ReferenceToExternalType absPath) =
        TR.moduleNmToQualTypeName . pathToModuleName $ absPath
    typeRefToQualTypeName (TR.ReferenceToLocalType s) =
        let baseTypeName = capitalise $ TR.moduleNmToQualTypeName s
         in pkgName <> "." <> baseTypeName
    typeRefToQualTypeName (TR.ReferenceToPrimitiveType s) = s
    go :: TypeRep -> (HsDecl', [ImportDecl'])
    go tr
        | (TR.ProdType km) <- tr =
            (, locDepsFromRecordLike km) $ data' (fromString declName) [] [buildProdCon km] []
        | (TR.SumType km) <- tr =
            (, locDepsFromRecordLike km) $ data' (fromString declName) [] (buildSumCon's km) []
      where
        buildProdCon :: Map TR.FieldName TR.TypeRef -> ConDecl'
        buildProdCon km =
            recordCon (fromString declName) $
            (bimap (fromString . changeReservedNames) (fieldFromReference)) <$> Map.toList km
        buildSumCon's :: Map TR.FieldName TR.TypeRef -> [ConDecl']
        buildSumCon's km =
            let constructorFromPair k v =
                    prefixCon (fromString . changeReservedNames $ k) $
                    singleton . fieldFromReference $ v
             in (uncurry constructorFromPair) <$> Map.toList km
        locDepsFromRecordLike :: Map TR.FieldName TR.TypeRef -> [ImportDecl']
        locDepsFromRecordLike km = catMaybes $ typeRefToLocalDeps . snd <$> Map.toList km
          where
            typeRefToLocalDeps :: TR.TypeRef -> Maybe ImportDecl'
            typeRefToLocalDeps (TR.ExtRef _nlr) = Nothing
            typeRefToLocalDeps (TR.LocRef lr) =
                Just . qualified' . import' . fromString $
                pkgName ++ "." ++ (capitalise $ TR.unLocalRef lr)
        fieldFromReference :: TR.TypeRef -> Field
        fieldFromReference tr'@(TR.ExtRef _) =
            bangfield . capitalise $ typeRefToQualTypeName tr'
        fieldFromReference tr'@(TR.LocRef _) =
            bangfield .
            ((pkgName ++ ".") ++) . capitalise . TR.moduleNmToQualTypeName . TR.toString $
            tr'
        bangfield :: String -> Field
        bangfield = strict . field . var . fromString
        changeReservedNames :: String -> String
        changeReservedNames "type" = "_type'"
        changeReservedNames "data" = "_data'"
        changeReservedNames x      = x
    go (TR.ArrayType tr) =
        localImports tr $
        type' (fromString declName) [] $
        var "Data.Vector.Vector" @@ (var . fromString . capitalise $ typeRefToQualTypeName tr)
      where
        localImports (TR.ExtRef _) = (, [])
        localImports (TR.LocRef _) =
            (, [import' . fromString . ((pkgName <> ".") <>) . capitalise . TR.toString $ tr])
    go (TR.NewType newtypeName tr) =
        (, []) $ newtype' typeName [] (recordCon constructorName [(fieldName, fieldType)]) []
      where
        typeName = fromString . capitalise $ newtypeName
        constructorName = typeName
        fieldName = fromString $ "un" ++ (capitalise newtypeName)
        fieldType = field . var $ fromString . capitalise $ typeRefToQualTypeName tr
    go (TR.Ref (TR.RefPrimitiveType s)) =
        (, []) $
        type'
            (fromString declName)
            []
            ((var . fromString $ declName) @@ (var . fromString . capitalise $ s))
    go (TR.Ref (TR.RefExternalType mn)) =
        (, []) $
        type'
            (fromString declName)
            []
            ((var . fromString $ declName) @@
             (var . fromString . capitalise . TR.moduleNmToQualTypeName $ (mn & dropExtension)))

buildExternalDep :: ModuleName -> Ctx (Map FilePath HsModule')
buildExternalDep moduleName = do
    state' <- get
    let moduleToBuild = Map.lookup (fromString moduleName) (includes state')
    case moduleToBuild of
        Nothing ->
            throwError $
            "While buildig external dependency" ++
            show moduleName ++ " did not find it among includes " ++ show state'
        Just Built -> return mempty
        Just (ToBuild yetTobuild) -> do
            let modulePrefix =
                    capitalise <$> (reverse $ split '/' (moduleName & dropExtension))
            let (newState :: GeneratorState) =
                    coerce . Map.delete (fromString moduleName) $
                    (coerce state' :: Map FilePath (Dep ModuleParts))
            put newState
            builtModules <- local (const modulePrefix) $ modulePartsToModules yetTobuild
            modify $ \incs ->
                GeneratorState $ Map.insert (fromString moduleName) Built $ coerce incs
            return builtModules

buildOrphanDeps :: Ctx (Map FilePath HsModule')
buildOrphanDeps = do
    nms <- fmap fst . filter f . Map.toList . includes <$> get
    mconcat <$> (traverse buildExternalDep $ nms)
  where
    f (_, ToBuild _) = True
    f (_, Built)     = False

modulePartsToModules :: ModuleParts -> Ctx (Map FilePath HsModule')
modulePartsToModules = Hylo.hylo breakDown buildUp

build :: ParserResult -> Either String (Map FilePath HsModule')
build (ParserResult mp deps) = do
    (km, _) <-
        runExcept $
        runStateT
            (runReaderT
                 (do res <- modulePartsToModules mp
                     orphKM <- buildOrphanDeps
                     return $ res <> orphKM)
                 mempty) $
        GeneratorState . Map.fromList $ Data.Bifunctor.bimap fromString ToBuild <$> deps
    return km

{-# LANGUAGE RecordWildCards #-}

module Data.ConfigGen.Traverse where

import Control.Monad.Except       (Except, MonadError (..))
import Control.Monad.Reader       (MonadReader (..), ReaderT (..), asks, local, withReaderT)
import Control.Monad.State.Strict (MonadState (..), StateT (..), modify)

import           Data.Bifunctor  (bimap)
import qualified Data.Bifunctor
import           Data.Coerce     (coerce)
import           Data.List       (foldl', intersperse)
import qualified Data.Map.Strict as M
import           Data.Maybe      (catMaybes)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.String     (fromString)
import           System.FilePath (dropExtension, (<.>), (</>))
import           Util            (capitalise, singleton, split)

import qualified Data.Aeson.Key    as K
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM

import           Data.ConfigGen.Parsing       (Title)
import qualified Data.ConfigGen.Traverse.Hylo as Hylo
import           Data.ConfigGen.TypeRep       (ModuleName, ModuleParts (ModuleParts), TypeRep)
import qualified Data.ConfigGen.TypeRep       as TR

import Data.Function ((&))
import GHC.SourceGen (App ((@@)), ConDecl', Field, HsDecl', HsModule', ImportDecl', Var (var),
                      data', field, import', module', newtype', prefixCon, qualified',
                      recordCon, strict, type')

type ModulePrefix = [String]

type PackageName = String

type DeclName = String

data Dep a
    = Built
    | ToBuild a
    deriving (Show, Eq, Ord)

newtype GeneratorState =
    GeneratorState
        { includes :: KeyMap (Dep ModuleParts)
        }
    deriving newtype (Semigroup, Monoid, Show)

type Ctx a = ReaderT ModulePrefix (StateT GeneratorState (Except String)) a

data NodeF a s
    = Local a (KeyMap s)
    | Leaf a

getPayload :: NodeF a s -> a
getPayload (Local p _) = p
getPayload (Leaf p)    = p

data Payload =
    Payload
        { title        :: Maybe String
        , externalDeps :: Set ModuleName
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

buildUp :: Hylo.Algebra (NodeF Payload) (Ctx (KeyMap HsModule'))
buildUp node = do
    let p@(Payload title externalDeps _) = getPayload node
    extDeps <- mconcat <$> (traverse buildExternalDep $ (Set.toList externalDeps))
    path <- asks $ K.fromString . extractPath title
    (fullModuleName, declName) <- asks $ extractFullPackageName title
    appendLocalDeps <-
        case node of
            Local _ km -> do
                built <-
                    fmap (KM.foldl' (<>) mempty) . sequence $
                    KM.fromMap . M.mapWithKey (withReaderT . updatePrefix title) . KM.toMap $
                    km
                return $ (<> built)
            Leaf _ -> return id
    return . (<> extDeps) . appendLocalDeps . KM.singleton path $
        buildModule fullModuleName declName p

updatePrefix :: Maybe Title -> K.Key -> ModulePrefix -> ModulePrefix
updatePrefix (Just title) fieldName [] =
    [capitalise . K.toString $ fieldName, capitalise title]
updatePrefix _ fieldName prefix = (capitalise . K.toString $ fieldName) : prefix

extractPath :: Maybe Title -> ModulePrefix -> FilePath
extractPath Nothing prefix = (foldl' (</>) mempty . reverse $ prefix) <.> "hs"
extractPath (Just title) prefix =
    (foldl' (</>) mempty . reverse . drop 1 $ prefix) </> capitalise title <.> "hs"

extractFullPackageName :: Maybe Title -> ModulePrefix -> (PackageName, DeclName)
extractFullPackageName Nothing prefix =
    (mconcat . intersperse "." . reverse $ prefix, takeHeadOrFail prefix)
  where
    takeHeadOrFail (x:_) = x
    takeHeadOrFail _     = "Unnamed" -- error "Prefix is emtpy, no Name to give"
extractFullPackageName (Just title) prefix =
    ( mconcat . intersperse "." $ reverse (capitalise title : drop 1 prefix)
    , capitalise $ title)

pathToModuleName :: String -> String
pathToModuleName s = (mconcat . intersperse "." $ capitalise <$> split '/' s) & dropExtension

-- It might be fairly easy to analyse TypeRef for primitive ref usage and then there will be no need for this.
defaultImports :: [ImportDecl']
defaultImports =
    qualified' . import' . fromString <$> ["GHC.Types", "GHC.Int", "Data.Text", "Data.Vector"]

buildModule :: PackageName -> DeclName -> Payload -> HsModule'
buildModule pkgName declName Payload {..} =
    let exports = Nothing
        extImports =
            qualified' . import' . fromString . pathToModuleName <$> Set.toList externalDeps
        (decls, locImports) = Data.Bifunctor.first singleton $ go typeRep
     in module'
            (Just . fromString $ pkgName)
            exports
            (extImports <> locImports <> defaultImports)
            decls
  where
    typeRefToQualTypeName (TR.ReferenceToExternalType s) =
        TR.moduleNmToQualTypeName . dropExtension $ s
    typeRefToQualTypeName (TR.ReferenceToLocalType s) = TR.moduleNmToQualTypeName s
    typeRefToQualTypeName (TR.ReferenceToPrimitiveType s) = s
    go :: TypeRep -> (HsDecl', [ImportDecl'])
    go tr
        | (TR.ProdType km) <- tr =
            (, locDepsFromRecordLike km) $ data' (fromString declName) [] [buildProdCon km] []
        | (TR.SumType km) <- tr =
            (, locDepsFromRecordLike km) $ data' (fromString declName) [] (buildSumCon's km) []
      where
        buildProdCon :: KeyMap TR.TypeRef -> ConDecl'
        buildProdCon km =
            recordCon (fromString declName) $
            (bimap (fromString . K.toString) (fieldFromReference)) <$> KM.toList km
        buildSumCon's :: KeyMap TR.TypeRef -> [ConDecl']
        buildSumCon's km =
            let constructorFromPair k v =
                    prefixCon (fromString . K.toString $ k) $
                    singleton . fieldFromReference $ v
             in (uncurry constructorFromPair) <$> KM.toList km
        locDepsFromRecordLike :: KeyMap TR.TypeRef -> [ImportDecl']
        locDepsFromRecordLike km = catMaybes $ typeRefToLocalDeps . snd <$> KM.toList km
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
    go (TR.ArrayType tr) =
        localImports tr $
        type' (fromString declName) [] $
        var "Data.Vector.Vector" @@ (var . fromString . capitalise $ typeRefToQualTypeName tr)
      where
        localImports (TR.ExtRef _) = (, [])
        localImports (TR.LocRef _) = (, [import' . fromString . capitalise . TR.toString $ tr])
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

buildExternalDep :: ModuleName -> Ctx (KeyMap HsModule')
buildExternalDep moduleName = do
    state' <- get
    let moduleToBuild = KM.lookup (fromString moduleName) (includes state')
    case moduleToBuild of
        Nothing ->
            throwError $
            "While buildig external dependency" ++
            show moduleName ++ " did not find it among includes " ++ show state'
        Just Built -> return mempty
        Just (ToBuild yetTobuild) -> do
            let (newState :: GeneratorState) =
                    coerce . KM.delete (fromString moduleName) $
                    (coerce state' :: KeyMap (Dep ModuleParts))
            put newState
            builtModules <- local (const mempty) $ modulePartsToModules yetTobuild
            modify $ \incs ->
                GeneratorState $ KM.insert (fromString moduleName) Built $ coerce incs
            return builtModules

modulePartsToModules :: ModuleParts -> Ctx (KeyMap HsModule')
modulePartsToModules = Hylo.hylo breakDown buildUp

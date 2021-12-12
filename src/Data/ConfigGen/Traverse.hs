{-# LANGUAGE RecordWildCards #-}

module Data.ConfigGen.Traverse where

import Control.Monad.Reader       (MonadReader (..), ReaderT (..), asks, withReaderT)
import Control.Monad.State.Strict (MonadState (..), StateT (..), modify)

import Data.List (foldl', intersperse)
import Data.Set  (Set)
import Util      (capitalise, singleton)

import Data.ConfigGen.TypeRep (ModuleName, ModuleParts (ModuleParts), TypeRep)
import GHC.SourceGen          (App ((@@)), HsDecl', HsModule', Var (var), data', field,
                               import', module', newtype', prefixCon, recordCon, strict, type')

import qualified Data.Aeson.Key    as K
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM

import           Control.Monad.Except         (Except, MonadError (throwError))
import           Data.ConfigGen.Parsing       (Title)
import qualified Data.ConfigGen.Traverse.Hylo as Hylo
import           Data.Foldable                (traverse_)
import qualified Data.Map.Strict              as M
import qualified Data.Set                     as Set
import           Data.String                  (fromString)
import           System.FilePath              ((<.>), (</>))

import           Data.Coerce            (coerce)
import qualified Data.ConfigGen.TypeRep as TR

type ModulePrefix = [String]

type PackageName = String

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
buildUp (Leaf p@(Payload title externalDeps _)) = do
    extDeps <- mconcat <$> (traverse buildExternalDep $ (Set.toList externalDeps))
    path <- asks $ K.fromString . extractPath title
    (fullModuleName, declName) <- asks $ extractFullPackageName title
    return . (<> extDeps) . KM.singleton path $ buildModule fullModuleName declName p
buildUp (Local p@(Payload title externalDeps _) km) = do
    built <-
        fmap (KM.foldl' (<>) mempty) . sequence $
        KM.fromMap . M.mapWithKey (withReaderT . (\k mp -> K.toString k : mp)) . KM.toMap $ km
    extDeps <- mconcat <$> (traverse buildExternalDep $ (Set.toList externalDeps))
    path <- asks $ K.fromString . extractPath title
    (fullModuleName, declName) <- asks $ extractFullPackageName title
    return . (<> extDeps) . (<> built) . KM.singleton path $
        buildModule fullModuleName declName p

extractPath :: Maybe Title -> ModulePrefix -> FilePath
extractPath Nothing prefix = (foldl' (</>) mempty . reverse $ prefix) <.> "hs"
extractPath (Just title) prefix =
    (foldl' (</>) mempty . reverse . drop 1 $ prefix) </> title <.> "hs"

extractFullPackageName :: Maybe Title -> ModulePrefix -> (PackageName, String)
extractFullPackageName Nothing prefix =
    (mconcat . intersperse "." . fmap capitalise . reverse $ prefix, last prefix) -- <- here will be an error if top level package has no title
extractFullPackageName (Just title) prefix =
    (mconcat . intersperse "." . fmap capitalise $ reverse (title : drop 1 prefix), title)

buildModule :: PackageName -> String -> Payload -> HsModule'
buildModule pkgName declName Payload {..} =
    let exports = Nothing
        imports = import' . fromString <$> Set.toList externalDeps
        decls = singleton $ go typeRep
     in module' (Just . fromString $ pkgName) exports imports decls
  where
    go :: TypeRep -> HsDecl'
    go tr
        | (TR.ProdType km) <- tr =
            data'
                (fromString declName)
                []
                [ recordCon (fromString declName) $
                  (\(k, v) ->
                       ( fromString . K.toString $ k
                       , field . var . fromString . TR.getNameFromReference $ v)) <$>
                  KM.toList km
                ]
                []
        | (TR.SumType km) <- tr =
            data'
                (fromString declName)
                []
                ((\(k, v) ->
                      prefixCon (fromString . K.toString $ k) $
                      singleton . strict . field . var . fromString . TR.getNameFromReference $
                      v) <$>
                 KM.toList km)
                []
    go (TR.ArrayType tr) =
        type' (fromString declName) [] $
        var "Data.Vector.Vector" @@ (var . fromString . TR.getNameFromReference $ tr)
    go (TR.NewType newtypeName nlr) =
        newtype'
            (fromString newtypeName)
            []
            (recordCon constructorName [(fieldName, fieldType)])
            []
      where
        typeName = fromString newtypeName
        constructorName = typeName
        fieldName = fromString $ "un" ++ newtypeName
        fieldType = strict . field . var $ fromString . TR.getNameFromReference $ nlr
    go (TR.Ref (TR.RefPrimitiveType s)) =
        type'
            (fromString declName)
            []
            ((var . fromString $ declName) @@ (var . fromString $ s))
    go (TR.Ref (TR.RefExternalType mn)) =
        type'
            (fromString declName)
            []
            ((var . fromString $ declName) @@ (var . fromString $ mn))

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
            modify $ \(GeneratorState incs) ->
                GeneratorState $ KM.insert (fromString moduleName) Built incs
            return builtModules

modulePartsToModules :: ModuleParts -> Ctx (KeyMap HsModule')
modulePartsToModules = Hylo.hylo breakDown buildUp

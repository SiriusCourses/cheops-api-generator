{-# LANGUAGE RecordWildCards #-}

module Data.ConfigGen.Traverse where

import Control.Monad.Reader       (ReaderT, asks, withReaderT)
import Control.Monad.State.Strict (StateT, modify)

import Data.List (intersperse)
import Data.Set  (Set)
import Util      (capitalise)

import Data.ConfigGen.TypeRep (ModuleName, ModuleParts (ModuleParts), TypeRep)
import GHC.SourceGen          (HsDecl', HsModule', HsType', module')

import qualified Data.Aeson.Key    as K
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM

import           Data.ConfigGen.Parsing       (Title)
import qualified Data.ConfigGen.Traverse.Hylo as Hylo
import qualified Data.Set                     as Set
import           Data.String                  (fromString)
import           Util                         (singleton)

type ModulePrefix = [String]

data ConstructedType =
    ConstructedType
        { _constrType :: HsType'
        , _constrDecl :: HsDecl'
        }

newtype GeneratorState =
    GeneratorState
        { includes :: KeyMap (Either ModuleParts HsModule')
        }

type Ctx a = ReaderT ModulePrefix (StateT GeneratorState (Either String)) a

data NodeF a s
    = Local a (KeyMap s)
    | Leaf a

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
    modify $ buildExternalDeps externalDeps
    namespace <- asks $ mconcat . intersperse "." . fmap capitalise
    let moduleName = makeModuleName title namespace
    path <- asks $ K.fromString . _make_path_out_of_list_of_strings
    return . KM.singleton path $ buildModule moduleName p
buildUp (Local p@(Payload title externalDeps _) km) = do
    built <-
        fmap (KM.foldMapWithKey (\_ x -> x)) . sequence $
        withReaderT _pass_proper_prefix <$> km
    modify $ buildExternalDeps externalDeps
    path <- asks $ K.fromString . _make_path_out_of_list_of_string
    namespace <- asks $ mconcat . intersperse "." . fmap capitalise
    let moduleName = makeModuleName title namespace
    return . (<> built) . KM.singleton path $ buildModule moduleName p

buildModule :: String -> Payload -> HsModule'
buildModule name Payload {..} =
    let exports = Nothing
        imports = _ <$> Set.toList externalDeps
        decls = singleton $ _ typeRep
     in module' (Just . fromString $ name) exports imports decls

makeModuleName :: Maybe Title -> String -> ModuleName
makeModuleName title prefix = _append_in_proper_fashion

buildExternalDeps :: Set ModuleName -> GeneratorState -> GeneratorState
buildExternalDeps =
    _check_external_dependencies_and_construct_modules_from_them_if_they_are_not_constucted_already

meh = Hylo.hylo breakDown buildUp

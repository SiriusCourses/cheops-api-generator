module Data.ConfigGen.Traverse where

import Control.Monad.Reader       (ReaderT, asks)
import Control.Monad.State.Strict (StateT, modify)

import Data.List (intersperse)
import Data.Set  (Set)
import Util      (capitalise)

import Data.ConfigGen.TypeRep (ModuleName, ModuleParts (ModuleParts), TypeRep)
import GHC.SourceGen          (HsDecl', HsModule', HsType')

import qualified Data.Aeson.Key    as K
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM

import qualified Data.ConfigGen.Traverse.Hylo as Hylo

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
buildUp (Leaf (Payload title externalDeps typeRep)) = do
    namespace <- asks $ mconcat . intersperse "." . fmap capitalise
    modify $ buildExternalDeps externalDeps
    return $ KM.singleton (K.fromString namespace) _
buildUp (Local payload km) = do
    built <- sequence $ _update_keys_with_prefix <$> km
    modify $ buildExternalDeps (externalDeps payload)
    return undefined

buildExternalDeps :: Set ModuleName -> GeneratorState -> GeneratorState
buildExternalDeps =
    _check_external_dependencies_and_construct_modules_from_them_if_they_are_not_constucted_already

meh = Hylo.hylo breakDown buildUp

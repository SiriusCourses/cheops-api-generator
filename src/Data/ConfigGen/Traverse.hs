{-# LANGUAGE RecordWildCards #-}

module Data.ConfigGen.Traverse where

import Control.Monad.Reader       (ReaderT (runReaderT), asks, withReaderT)
import Control.Monad.State.Strict (StateT (runStateT), execStateT, modify)

import Data.List (foldl', intersperse)
import Data.Set  (Set)
import Util      (capitalise, singleton)

import Data.ConfigGen.TypeRep (ModuleName, ModuleParts (ModuleParts), TypeRep)
import GHC.SourceGen          (HsDecl', HsModule', HsType', import', module')

import qualified Data.Aeson.Key    as K
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM

import           Control.Monad                (foldM)
import           Data.ConfigGen.Parsing       (Title)
import qualified Data.ConfigGen.Traverse.Hylo as Hylo
import qualified Data.Map.Strict              as M
import           Data.Maybe                   (fromMaybe)
import qualified Data.Set                     as Set
import           Data.String                  (fromString)
import           Data.Traversable             (for)
import           System.FilePath              ((<.>), (</>))

type ModulePrefix = [String]

newtype GeneratorState =
    GeneratorState
        { includes :: KeyMap (Either ModuleParts HsModule')
        } deriving newtype (Semigroup, Monoid)

instance Show GeneratorState where
    show (GeneratorState incs) =
        "GeneratorState" ++
        (show $
         (\case
              Left x  -> "Left " ++ show x
              Right _ -> "Right Hs.. staff`") <$>
         incs)

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
    modify $ buildExternalDeps (Set.toList externalDeps)
    path <- asks $ K.fromString . extractPath title
    fullModuleName <- asks $ extractFullPackageName title
    return . KM.singleton path $ buildModule fullModuleName p
buildUp (Local p@(Payload title externalDeps _) km) = do
    built <-
        fmap (KM.foldl' (<>) mempty) . sequence $
        KM.fromMap . M.mapWithKey (withReaderT . (\k mp -> K.toString k : mp)) . KM.toMap $ km
    modify $ buildExternalDeps (Set.toList externalDeps)
    path <- asks $ K.fromString . extractPath title
    moduleName <- asks $ extractFullPackageName title
    return . (<> built) . KM.singleton path $ buildModule moduleName p

extractPath :: Maybe Title -> ModulePrefix -> String
extractPath Nothing prefix = (foldl' (</>) mempty . reverse $ prefix) <.> "hs"
extractPath (Just title) prefix =
    (foldl' (</>) mempty . reverse . drop 1 $ prefix) </> title <.> "hs"

extractFullPackageName :: Maybe Title -> ModulePrefix -> String
extractFullPackageName Nothing prefix =
    mconcat . intersperse "." . fmap capitalise . reverse $ prefix
extractFullPackageName (Just title) prefix =
    mconcat . intersperse "." . fmap capitalise $ reverse (title : drop 1 prefix)

buildModule :: String -> Payload -> HsModule'
buildModule name Payload {..} =
    let exports = Nothing
        imports = import' . _ <$> Set.toList externalDeps
        decls = singleton $ _ typeRep
     in module' (Just . fromString $ name) exports imports decls

buildExternalDeps :: [ModuleName] -> GeneratorState -> GeneratorState
buildExternalDeps modules modulePool =
    foldl'
        (\modulePool m ->
             let item = KM.lookup (fromString m) $ includes modulePool
              in case item of
                     Nothing ->
                         error $
                         "Something went very wrong. Dependency `" ++
                         m ++ "` is not present in global dependecy map: " ++ show modulePool
                     Just (Left mp) ->
                         (case runStateT (runReaderT (modulePartsToModules mp) []) modulePool of
                              Left s ->
                                  error $
                                  "Something is very wrong. Tried to build " ++
                                  show m ++
                                  " got error" ++ s ++ ". Relevant context" ++ show modulePool
                              Right (res, state) ->
                                  GeneratorState .
                                  (\t -> KM.insert (fromString m) t $ includes (state <> modulePool)) $
                                  _??????_)
                     Just (Right _) -> modulePool)
        modulePool
        modules
    -- _check_external_dependencies_and_construct_modules_from_them_if_they_are_not_constucted_already

modulePartsToModules :: ModuleParts -> Ctx (KeyMap HsModule')
modulePartsToModules = Hylo.hylo breakDown buildUp

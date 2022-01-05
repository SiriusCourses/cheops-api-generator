module Data.TransportTypes.CodeGen.Hylo.BuildUp where

import Control.Monad.Except       (Except, MonadError (..), runExcept)
import Control.Monad.Reader       (ReaderT (..), asks, withReaderT)
import Control.Monad.State.Strict (MonadState (..), StateT (..), modify)

import           Data.Bifunctor (bimap)
import           Data.Coerce    (coerce)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import qualified Data.Set       as Set
import           Data.String    (fromString)

import           Data.TransportTypes.CodeGen.Hylo.Internal  (Algebra, hylo)
import           Data.TransportTypes.CodeGen.Hylo.Structure (NodeF (..), Payload (..), getPayload)
import qualified Data.TransportTypes.CodeGen.NamingUtils    as U
import qualified Data.TransportTypes.ModuleParts            as MP
import           Data.TransportTypes.Parsing                (ParserResult (ParserResult))

import Data.TransportTypes.CodeGen.Hylo.BreakDown (breakDown)
import GHC.SourceGen                         (HsModule')

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

buildUp ::
       (Payload -> U.ModulePrefix -> HsModule')
    -> Algebra (NodeF Payload) (Ctx (Map FilePath HsModule'))
buildUp k node = do
    let p@(Payload _ externalDeps' _) = getPayload node
    localDeps <-
        case node of
            Local _ km ->
                fmap (Map.foldl' (<>) mempty) . sequence $
                Map.mapWithKey (withReaderT . U.updatePrefix) km
            Leaf _ -> return mempty
    extDeps <- mconcat <$> traverse (buildExternalDep k) (Set.toList externalDeps')
    newModule <- asks $ k p
    path <- asks U.prefixToPath
    return $ Map.singleton path newModule <> extDeps <> localDeps

buildExternalDep ::
       (Payload -> U.ModulePrefix -> HsModule') -> FilePath -> Ctx (Map FilePath HsModule')
buildExternalDep k path = do
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
            builtModules <-
                withReaderT (const modulePrefix) $ modulePartsToModules k yetTobuild
            modify $ \incs -> GeneratorState $ Map.insert (fromString path) Built $ coerce incs
            return builtModules

buildOrphanDeps :: (Payload -> U.ModulePrefix -> HsModule') -> Ctx (Map FilePath HsModule')
buildOrphanDeps k = do
    nms <- fmap fst . filter f . Map.toList . includes <$> get
    mconcat <$> traverse (buildExternalDep k) nms
  where
    f (_, ToBuild _) = True
    f (_, Built)     = False

modulePartsToModules ::
       (Payload -> U.ModulePrefix -> HsModule')
    -> MP.ModuleParts
    -> Ctx (Map FilePath HsModule')
modulePartsToModules k = hylo breakDown (buildUp k)

build ::
       (Payload -> U.ModulePrefix -> HsModule')
    -> ParserResult
    -> Either String (Map FilePath HsModule')
build k (ParserResult _ deps) = do
    (km, _) <-
        runExcept $
        runStateT (runReaderT (buildOrphanDeps k) mempty) $
        GeneratorState . Map.fromList $ Data.Bifunctor.bimap fromString ToBuild <$> deps
    return km

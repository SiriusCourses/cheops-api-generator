{-# LANGUAGE RecordWildCards #-}

module Data.TransportTypes.CodeGen.TypeGen where

import Control.Monad ((<=<))

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes, mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set

import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import qualified Data.TransportTypes.TypeRep             as TR

import           Data.Bifunctor (bimap)
import           Data.String    (fromString)
import qualified Data.Text      as T

import GHC.SourceGen (App ((@@)), ConDecl', Field, HsDecl', HsModule', HsType', Var (var),
                      data', field, import', module', newtype', prefixCon, qualified',
                      recordCon, strict)

import           Control.Monad.Reader                            (Reader, asks, runReader)
import           Data.Foldable                                   (toList)
import           Data.TransportTypes.CodeGen.Hylo                (Payload (..))
import           Data.TransportTypes.CodeGen.TypeGen.InstanceGen (buildFromJSONInstance,
                                                                  buildToJSONInstance,
                                                                  buildToSchemaInstance)
import qualified Data.TransportTypes.CodeGen.Utils               as U
import           Data.Yaml                                       (Value (..))

type TypeCtx a = Reader (U.ModulePrefix, U.QualTypeName) a

askQualTypename :: Reader (U.ModulePrefix, U.QualTypeName) U.QualTypeName
askQualTypename = asks snd

askModulePrefix :: Reader (U.ModulePrefix, U.QualTypeName) U.ModulePrefix
askModulePrefix = asks fst

askTypename :: Reader (U.ModulePrefix, U.QualTypeName) TR.TypeName
askTypename = asks (U.typenameFromQualTypeName . snd)

gatherLocalImports :: U.ModulePrefix -> TR.TypeRep -> [TR.ModuleName]
gatherLocalImports prefix tr
    | TR.AllOfType set <- tr = gather set
    | TR.AnyOfType set <- tr = gather set
  where
    gather :: Set TR.TypeRef -> [String]
    gather set = catMaybes . Set.toList $ Set.map (U.referenceToModuleName prefix) set
gatherLocalImports prefix tr
    | (TR.ProdType map') <- tr = gatherProd map'
    | (TR.SumType map') <- tr = gatherSum map'
    | (TR.OneOfType map') <- tr = gatherSum map'
  where
    gatherSum :: Map U.FieldName TR.SumConstr -> [TR.ModuleName]
    gatherSum = catMaybes . snd <=< Map.toList . fmap TR.unSumConstr . Map.map (fmap go)
      where
        go :: TR.Field -> Maybe TR.ModuleName
        go (TR.Field _ tr') = U.referenceToModuleName prefix tr'
    gatherProd :: Map U.FieldName TR.Field -> [TR.ModuleName]
    gatherProd = mapMaybe snd . Map.toList . Map.map go
      where
        go :: TR.Field -> Maybe TR.ModuleName
        go (TR.Field _ tr') = U.referenceToModuleName prefix tr'
gatherLocalImports prefix (TR.ArrayType tr') = toList $ U.referenceToModuleName prefix tr'
gatherLocalImports prefix (TR.NewType tr') = toList $ U.referenceToModuleName prefix tr'
gatherLocalImports _prefix (TR.Ref nlr) = toList $ U.nonLocalReferenceToModuleName nlr
gatherLocalImports _ _ = mempty

buildModule :: Payload -> U.ModulePrefix -> HsModule'
buildModule Payload {..} prefix =
    let extImports =
            qualified' . import' . fromString . U.prefixToModuleName . U.pathToPrefix <$>
            Set.toList externalDeps
        exports = Nothing
        defaultImports = qualified' . import' . fromString <$> U.defaultImportNames
        locals = qualified' . import' . fromString <$> gatherLocalImports prefix typeRep
     in module'
            (Just . fromString $ U.prefixToModuleName prefix)
            exports
            (extImports <> locals <> defaultImports <> [U.hidingPrelude])
            (tDecl : instances)
  where
    qualTypename :: TR.TypeName
    qualTypename = U.prefixToQualTypeName prefix title
    typename = U.typenameFromQualTypeName qualTypename
    tDecl = runReader (buildTypeDecl typeRep) (prefix, qualTypename)
    instances =
        [ buildToJSONInstance typename typeRep
        , buildFromJSONInstance typename typeRep
        , buildToSchemaInstance title prefix typename typeRep
        ]

transformField :: U.ModulePrefix -> TR.Field -> Field
transformField prefix (TR.Field req tr) =
    let tt = var . fromString $ U.referenceToQualTypeName prefix tr
     in strict . field $ maybeWrapper req tt
  where
    maybeWrapper :: Bool -> HsType' -> HsType'
    maybeWrapper False = (var "Prelude.Maybe" @@)
    maybeWrapper True  = id

buildTypeDecl :: TR.TypeRep -> TypeCtx HsDecl'
buildTypeDecl (TR.AnyOfType set) = do
    typename <- askTypename
    prefix <- askModulePrefix
    let fields =
            field .
            (var "Prelude.Maybe" @@) . var . fromString . U.referenceToQualTypeName prefix <$>
            Set.toList set
    return $
        data'
            (fromString typename)
            []
            [prefixCon (fromString typename) fields]
            U.minDerivingClause
buildTypeDecl (TR.AllOfType set) = do
    typename <- askTypename
    prefix <- askModulePrefix
    let fields = field . var . fromString . U.referenceToQualTypeName prefix <$> Set.toList set
    return $
        data'
            (fromString typename)
            []
            [prefixCon (fromString typename) fields]
            U.minDerivingClause
buildTypeDecl (TR.ProdType map') = do
    typename <- askTypename
    prodCntr <- buildProdCon map'
    return $ data' (fromString typename) [] [prodCntr] U.minDerivingClause
  where
    buildProdCon :: Map TR.FieldName TR.Field -> TypeCtx ConDecl'
    buildProdCon km = do
        typename <- askTypename
        prefix <- askModulePrefix
        return $
            recordCon (fromString typename) $
            bimap (fromString . U.changeReservedNames) (transformField prefix) <$>
            Map.toList km
buildTypeDecl tr
    | (TR.SumType map') <- tr = do
        typename <- askTypename
        prefix <- askModulePrefix
        return $ data' (fromString typename) [] (buildSumCon's prefix map') U.minDerivingClause
    | (TR.OneOfType map') <- tr = do
        typename <- askTypename
        prefix <- askModulePrefix
        return $
            data' (fromString typename) [] (buildSumCon's prefix map') U.minDerivingClause
  where
    buildSumCon's :: U.ModulePrefix -> Map TR.FieldName TR.SumConstr -> [ConDecl']
    buildSumCon's prefix km =
        let constructorFromPair :: String -> TR.SumConstr -> ConDecl'
            constructorFromPair k v =
                prefixCon (fromString . U.fieldNameToSumCon . U.changeReservedNames $ k) $
                TR.unSumConstr $ transformField prefix <$> v
         in uncurry constructorFromPair <$> Map.toList km
buildTypeDecl (TR.ArrayType tr') = do
    typename <- askTypename
    prefix <- askModulePrefix
    let listType = var "Data.Vector.Vector"
    let listItem = var . fromString $ U.referenceToQualTypeName prefix tr'
    return $
        newtype'
            (fromString typename)
            []
            (prefixCon (fromString typename) [field $ listType @@ listItem])
            U.minDerivingClause
buildTypeDecl (TR.NewType tr') = do
    typename <- askTypename
    prefix <- askModulePrefix
    let newtypename = fromString typename
    let constructorName = fromString typename
    let internalType = field . var . fromString $ U.referenceToQualTypeName prefix tr'
    let getter = fromString $ U.getterName typename
    return $
        newtype'
            newtypename
            []
            (recordCon constructorName [(getter, internalType)])
            U.minDerivingClause
buildTypeDecl (TR.Ref nlr) = do
    typename <- askTypename
    let symtype = var . fromString $ U.nonLocalReferenceToQualTypeName nlr
    return $
        newtype'
            (fromString typename)
            []
            (prefixCon (fromString typename) [field symtype])
            U.minDerivingClause
buildTypeDecl (TR.ConstType v) = do
    typename <- askTypename
    let cntrName =
            case v of
                String txt -> U.fieldNameToSumCon $ T.unpack txt
                _other     -> typename
    return $
        data' (fromString typename) [] [prefixCon (fromString cntrName) []] U.minDerivingClause

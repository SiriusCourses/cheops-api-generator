{-# LANGUAGE RecordWildCards #-}

module Data.TransportTypes.CodeGen
    ( buildTests
    , buildModules
    ) where

import Control.Monad ((<=<))
import Data.Foldable (toList)

import           Data.Bifunctor  (bimap)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes, mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.String     (fromString)

import GHC.SourceGen (App ((@@)), ConDecl', Field, HsDecl', HsModule', HsType', ImportDecl',
                      Var (var), data', field, import', module', newtype', prefixCon,
                      qualified', recordCon, strict, type')

import           Data.TransportTypes.CodeGen.Hylo        (Payload (..), build)
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import           Data.TransportTypes.Parsing             (ParserResult (..))
import qualified Data.TransportTypes.TypeRep             as TR

buildTest :: Payload -> U.ModulePrefix -> HsModule'
buildTest Payload {..} prefix = undefined

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
        | TR.AllOfType set <- tr =
            qualified' . import' . fromString <$> (gather set <> ["Data.TransportTypes.Deriv"])
        | TR.AnyOfType set <- tr =
            qualified' . import' . fromString <$> (gather set <> ["Data.TransportTypes.Deriv"])
      where
        gather :: Set TR.TypeRef -> [String]
        gather set = catMaybes . Set.toList $ Set.map (U.referenceToModuleName prefix) set
    gatherLocalImports tr
        | (TR.ProdType map') <- tr = gatherProd map'
        | (TR.SumType map') <- tr = gatherSum map'
        | (TR.OneOf map') <- tr =
            gatherSum map' <> (qualified' . import' <$> ["Data.TransportTypes.Deriv"])
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
        maybeWrapper False = (var "Maybe" @@)
        maybeWrapper True  = id
    buildTypeDecl :: TR.TypeRep -> HsDecl'
    buildTypeDecl (TR.AnyOfType set) =
        data'
            (fromString typename)
            []
            [prefixCon (fromString typename) fields]
            (U.aofDerivingClause typename)
      where
        fields =
            field . (var "Maybe" @@) . var . fromString . U.referenceToQualTypeName prefix <$>
            Set.toList set
    buildTypeDecl (TR.AllOfType set) =
        data'
            (fromString typename)
            []
            [prefixCon (fromString typename) fields]
            (U.aofDerivingClause typename)
      where
        fields = field . var . fromString . U.referenceToQualTypeName prefix <$> Set.toList set
    buildTypeDecl (TR.ProdType map') =
        data' (fromString typename) [] [buildProdCon map'] U.defaultDerivingClause
      where
        buildProdCon :: Map TR.FieldName TR.Field -> ConDecl'
        buildProdCon km =
            recordCon (fromString typename) $
            bimap (fromString . U.changeReservedNames) transformField <$> Map.toList km
    buildTypeDecl tr
        | (TR.SumType map') <- tr =
            data' (fromString typename) [] (buildSumCon's map') U.defaultDerivingClause
        | (TR.OneOf map') <- tr =
            data' (fromString typename) [] (buildSumCon's map') (U.aofDerivingClause typename)
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
            U.defaultDerivingClause
      where
        newtypename = fromString typename
        constructorName = fromString typename
        internalType = field . var . fromString $ U.referenceToQualTypeName prefix tr'
        getter = fromString $ U.getterName typename
    buildTypeDecl (TR.Ref nlr) = type' (fromString typename) [] symtype
      where
        symtype = var . fromString $ U.nonLocalReferenceToQualTypeName nlr

buildModules :: ParserResult -> Either String (Map FilePath HsModule')
buildModules = build buildModule

buildTests :: ParserResult -> Either String (Map FilePath HsModule')
buildTests = build buildTest

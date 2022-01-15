{-# LANGUAGE RecordWildCards #-}

module Data.TransportTypes.CodeGen.TypeGen where

import Control.Monad ((<=<))
import Data.Foldable (toList)

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

import GHC.SourceGen (App ((@@)), BVar (bvar), ConDecl', Field, HasList (list), HsDecl',
                      HsModule', HsType', RawMatch, Var (var), case', conP, data', field,
                      funBind, funBinds, import', instance', lambda, match, module', newtype',
                      prefixCon, qualified', recordCon, strict, string, tuple, tyApp, wildP)

import           Control.Monad.Reader              (Reader, asks, runReader)
import           Data.Text.Encoding                (decodeUtf8)
import           Data.TransportTypes.CodeGen.Hylo  (Payload (..))
import qualified Data.TransportTypes.CodeGen.Utils as U
import           Data.Yaml                         (Value (..), encode)

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
    | (TR.OneOf map') <- tr = gatherSum map'
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
        specialDerivImports =
            qualified' . import' . fromString <$>
            case typeRep of
                TR.AnyOfType _ -> ["Data.TransportTypes.Deriv"]
                TR.AllOfType _ -> ["Data.TransportTypes.Deriv"]
                _else          -> []
     in module'
            (Just . fromString $ U.prefixToModuleName prefix)
            exports
            (extImports <> locals <> defaultImports <> [U.hidingPrelude] <> specialDerivImports)
            (tDecl : instances)
  where
    qualTypename :: TR.TypeName
    qualTypename = U.prefixToQualTypeName prefix title
    typename = U.typenameFromQualTypeName qualTypename
    tDecl = runReader (buildTypeDecl typeRep) (prefix, qualTypename)
    instances =
        case typeRep of
            TR.ProdType _  -> [buildToJSONInstance typename typeRep]
            TR.SumType _   -> [buildToJSONInstance typename typeRep]
            TR.Const _     -> [buildToJSONInstance typename typeRep]
            TR.OneOf _     -> [buildToJSONInstance typename typeRep]
            TR.NewType _   -> [buildToJSONInstance typename typeRep]
            TR.ArrayType _ -> [buildToJSONInstance typename typeRep]
            TR.Ref _       -> [buildToJSONInstance typename typeRep]
            _other         -> []

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
            (U.aofDerivingClause typename)
buildTypeDecl (TR.AllOfType set) = do
    typename <- askTypename
    prefix <- askModulePrefix
    let fields = field . var . fromString . U.referenceToQualTypeName prefix <$> Set.toList set
    return $
        data'
            (fromString typename)
            []
            [prefixCon (fromString typename) fields]
            (U.aofDerivingClause typename)
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
    | (TR.OneOf map') <- tr = do
        typename <- askTypename
        prefix <- askModulePrefix
        return $ data' (fromString typename) [] (buildSumCon's prefix map') U.minDerivingClause
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
buildTypeDecl (TR.Const v) = do
    typename <- askTypename
    let cntrName =
            case v of
                String txt -> U.fieldNameToSumCon $ T.unpack txt
                _other     -> typename
    return $
        data' (fromString typename) [] [prefixCon (fromString cntrName) []] U.minDerivingClause

buildToJSONInstance :: TR.TypeName -> TR.TypeRep -> HsDecl'
buildToJSONInstance typename (TR.ProdType map') =
    instance'
        (var "Data.Yaml.ToJSON" @@ var (fromString typename))
        [funBind "toJSON" $ match [patternMatch] objectCntr]
  where
    patternMatch =
        conP (fromString typename) $ bvar . fromString . U.fieldNameToPatName <$> Map.keys map'
    objectCntr = var "Data.Yaml.object" @@ (var "Data.Maybe.catMaybes" @@ list pairs)
      where
        tupling key = lambda [bvar "x"] (tuple [string key, var "Data.Yaml.toJSON" @@ var "x"])
        myabeWrap req nm =
            let k = (fromString $ U.fieldNameToPatName nm)
             in if req
                    then var "Prelude.Just" @@ var k
                    else var "Prelude.id" @@ var k
        pairs =
            (\(k, TR.Field req _) -> var "Prelude.fmap" @@ tupling k @@ myabeWrap req k) <$>
            Map.toList map'
buildToJSONInstance typename (TR.SumType map') =
    instance'
        (var "Data.Yaml.ToJSON" @@ var (fromString typename))
        [funBinds "toJSON" $ uncurry mkClause <$> Map.toList map']
    -- fields are ignored as there is no sum type in json(besides OneOf which is represented by it's own tag).
    -- in future in migth be wise to make this other cases unrepresentable.
  where
    mkClause :: TR.FieldName -> TR.SumConstr -> RawMatch
    mkClause optName (TR.SumConstr _) = match [patternMatch] cnt
      where
        patternMatch =
            bvar (fromString . U.fieldNameToSumCon . U.changeReservedNames $ optName)
        cnt = var "Data.Yaml.toJSON" @@ (var "Data.Yaml.String" @@ string optName)
buildToJSONInstance typename (TR.OneOf map') =
    instance'
        (var "Data.Yaml.ToJSON" @@ var (fromString typename))
        [funBinds "toJSON" $ uncurry mkClause <$> Map.toList map']
    -- flds are treated if they have at most one entity as it is impossible to do otherwise in json
    -- in future in migth be wise to make other case unrepresentable.
  where
    mkClause :: TR.FieldName -> TR.SumConstr -> RawMatch
    mkClause optName (TR.SumConstr flds)
        | [] <- flds = match [wildP] $ string optName
        | otherwise =
            match [conP (fromString . U.fieldNameToSumCon $ optName) [bvar "x"]] $
            var "Data.Yaml.toJSON" @@ var "x"
buildToJSONInstance typename (TR.AnyOfType set') = undefined
buildToJSONInstance typename (TR.AllOfType set') = undefined
buildToJSONInstance typename (TR.ArrayType _) =
    instance' (var "Data.Yaml.ToJSON" @@ var (fromString typename)) [decl]
  where
    decl =
        funBind "toJSON" $
        match [conP (fromString typename) [bvar "vec"]] $
        var "Data.Yaml.Array" @@ (var "Prelude.fmap" @@ var "Data.Yaml.toJSON" @@ var "vec")
buildToJSONInstance typename (TR.NewType _) =
    instance' (var "Data.Yaml.ToJSON" @@ var (fromString typename)) [decl]
  where
    decl =
        funBind "toJSON" $
        match [conP (fromString typename) [bvar "x"]] $ var "Data.Yaml.toJSON" @@ var "x"
buildToJSONInstance typename (TR.Ref _) =
    instance' (var "Data.Yaml.ToJSON" @@ var (fromString typename)) [decl]
  where
    decl =
        funBind "toJSON" $
        match [conP (fromString typename) [bvar "x"]] $ var "Data.Yaml.toJSON" @@ var "x"
buildToJSONInstance typename (TR.Const va) =
    instance'
        (var "Data.Yaml.ToJSON" @@ var (fromString typename))
        [funBind "toJSON" $ match [wildP] decl]
  where
    decl =
        case'
            (var "Data.Yaml.decodeEither'" `tyApp` var "Data.Yaml.Value" @@
             string (T.unpack . decodeUtf8 . encode $ va))
            [ match [conP "Prelude.Left" [wildP]] $
              var "Prelude.error" @@ string "can't decode const value. Something is very wrong"
            , match [conP "Prelude.Right" [bvar "x"]] $ var "x"
            ]
-- buildFromJSONInstance :: TR.TypeRep -> HsDecl'
-- buildFromJSONInstance (TR.ProdType map)  = _wj
-- buildFromJSONInstance (TR.SumType map)   = _wk
-- buildFromJSONInstance (TR.OneOf map)     = _wl
-- buildFromJSONInstance (TR.AnyOfType set) = _wm
-- buildFromJSONInstance (TR.AllOfType set) = _wn
-- buildFromJSONInstance (TR.ArrayType tr') = _wo
-- buildFromJSONInstance (TR.NewType tr')   = _wp
-- buildFromJSONInstance (TR.Ref nlr)       = _wq
-- buildFromJSONInstance (TR.Const va)      = _w

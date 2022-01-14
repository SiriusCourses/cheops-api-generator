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
import qualified Data.Text       as T

import GHC.SourceGen

import           Data.Text.Encoding                      (decodeUtf8)
import           Data.TransportTypes.CodeGen.Hylo        (Payload (..), build)
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import           Data.TransportTypes.Parsing             (ParserResult (..))
import qualified Data.TransportTypes.TypeRep             as TR
import           Data.Yaml                               (Value (..), encode)

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
                TR.OneOf _     -> ["Data.TransportTypes.Deriv"]
                TR.AnyOfType _ -> ["Data.TransportTypes.Deriv"]
                TR.AllOfType _ -> ["Data.TransportTypes.Deriv"]
                _else          -> []
     in module'
            (Just . fromString $ U.prefixToModuleName prefix)
            exports
            (extImports <> locals <> defaultImports <> [U.hidingPrelude] <> specialDerivImports)
            (buildTypeDecl typeRep :
             case typeRep of
                 TR.ProdType _ -> [buildToJSONInstance typeRep]
                 TR.SumType _  -> [buildToJSONInstance typeRep]
                 TR.Const _    -> [buildToJSONInstance typeRep]
                 TR.OneOf _    -> [buildToJSONInstance typeRep]
                 _other        -> [])
  where
    typename :: TR.TypeName
    typename = U.prefixToTypeName prefix title
    transformField :: TR.Field -> Field
    transformField (TR.Field req tr) =
        let tt = var . fromString $ U.referenceToQualTypeName prefix tr
         in strict . field $ maybeWrapper req tt
      where
        maybeWrapper :: Bool -> HsType' -> HsType'
        maybeWrapper False = (var "Prelude.Maybe" @@)
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
            field .
            (var "Prelude.Maybe" @@) . var . fromString . U.referenceToQualTypeName prefix <$>
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
        data' (fromString typename) [] [buildProdCon map'] U.minDerivingClause
      where
        buildProdCon :: Map TR.FieldName TR.Field -> ConDecl'
        buildProdCon km =
            recordCon (fromString typename) $
            bimap (fromString . U.changeReservedNames) transformField <$> Map.toList km
    buildTypeDecl tr
        | (TR.SumType map') <- tr =
            data' (fromString typename) [] (buildSumCon's map') U.minDerivingClause
        | (TR.OneOf map') <- tr =
            data' (fromString typename) [] (buildSumCon's map') U.minDerivingClause
      where
        buildSumCon's :: Map TR.FieldName TR.SumConstr -> [ConDecl']
        buildSumCon's km =
            let constructorFromPair :: String -> TR.SumConstr -> ConDecl'
                constructorFromPair k v =
                    prefixCon (fromString . U.fieldNameToSumCon . U.changeReservedNames $ k) $
                    TR.unSumConstr $ transformField <$> v
             in uncurry constructorFromPair <$> Map.toList km
    buildTypeDecl (TR.ArrayType tr') =
        newtype'
            (fromString typename)
            []
            (prefixCon (fromString typename) [field $ listType @@ listItem])
            U.defaultDerivingClause
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
    buildTypeDecl (TR.Ref nlr) =
        newtype'
            (fromString typename)
            []
            (prefixCon (fromString typename) [field symtype])
            U.defaultDerivingClause
      where
        symtype = var . fromString $ U.nonLocalReferenceToQualTypeName nlr
    buildTypeDecl (TR.Const v) =
        data'
            (fromString typename)
            []
            [prefixCon (fromString cntrName) []]
            U.minDerivingClause
      where
        cntrName =
            case v of
                String txt -> U.fieldNameToSumCon $ T.unpack txt
                _other     -> typename
    buildToJSONInstance :: TR.TypeRep -> HsDecl'
    buildToJSONInstance (TR.ProdType map') =
        instance'
            (var "Data.Yaml.ToJSON" @@ var (fromString typename))
            [funBind "toJSON" $ match [patternMatch] objectCntr]
      where
        patternMatch =
            conP (fromString typename) $
            bvar . fromString . U.fieldNameToPatName <$> Map.keys map'
        objectCntr = var "Data.Yaml.object" @@ (var "Data.Maybe.catMaybes" @@ list pairs)
          where
            tupling key =
                lambda [bvar "x"] (tuple [string key, var "Data.Yaml.toJSON" @@ var "x"])
            myabeWrap req nm =
                let k = (fromString $ U.fieldNameToPatName nm)
                 in if req
                        then var "Prelude.Just" @@ var k
                        else var "Prelude.id" @@ var k
            pairs =
                (\(k, TR.Field req _) -> var "Prelude.fmap" @@ tupling k @@ myabeWrap req k) <$>
                Map.toList map'
    buildToJSONInstance (TR.SumType map') =
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
    buildToJSONInstance (TR.OneOf map') = instance' (var "Data.Yaml.ToJSON" @@ var (fromString typename))
            [funBinds "toJSON" $ uncurry mkClause <$> Map.toList map']
      where
        -- flds are treated if they have at most one entity as it is impossible to do otherwise in json
        -- in future in migth be wise to make other case unrepresentable.
        mkClause :: TR.FieldName -> TR.SumConstr -> RawMatch
        mkClause optName (TR.SumConstr flds)
          | [] <- flds = match [wildP] $ string optName
          | otherwise = match [conP (fromString . U.fieldNameToSumCon $ optName) [bvar "x"]] $ var "Data.Yaml.toJSON" @@ var "x"

    buildToJSONInstance (TR.AnyOfType set') = undefined
    buildToJSONInstance (TR.AllOfType set') = undefined
    buildToJSONInstance (TR.ArrayType tr') = undefined
    buildToJSONInstance (TR.NewType tr') = undefined
    buildToJSONInstance (TR.Ref nlr) = undefined
    buildToJSONInstance (TR.Const va) =
        instance'
            (var "Data.Yaml.ToJSON" @@ var (fromString typename))
            [funBind "toJSON" $ match [wildP] decl]
      where
        decl =
            case'
                (var "Data.Yaml.decodeEither'" `tyApp` var "Data.Yaml.Value" @@ string (T.unpack . decodeUtf8 . encode $ va)) $
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

{-
instance ToJSON Typename where
    toJSON (Typename k1 k2 ... kn) = Object (KM.fromList [($k1, toJSON k1), ... ($kn, toJSON kn)])
-}
{-
instance ToJSON Typename where
    toJSON (Option1 k) = toJSON k
    ...
    toJSON (OptionN k) = toJSON k
-}
{-
instance ToJSON Typename where
    toJSON (Option1 k) = toJSON k
    ...
    toJSON (OptionN k) = toJSON k
-}
{-
instance ToJSON Typename where
    toJSON (Typename Nothing Nothing Nothing Nothing ...) = Object $ mempty
    toJSON (Typename (Just k1) Nothing Nothing Nothing ...) = toJSON k1
    ...
    toJSON (Typename (Just k1) (Just k2) Nothing ...) = merge (toJSON k1) (toJSON k2)
-}
{-
instance ToJSON Typename where
    toJSON (Typename k1 k2 ...) = merge (toJSON k1) (toJSON k2) ...
-}
{-
instance ToJSON Typename where
    generic??
-}
{-
instance ToJSON Typename where
    discard newtype wrapper?
-}
{-
deriving newtype instance ToJSON Typename
-}
{-
instance ToJSON Typename where
    inline const?
-}
buildModules :: ParserResult -> Either String (Map FilePath HsModule')
buildModules = build buildModule

buildTests :: ParserResult -> Either String (Map FilePath HsModule')
buildTests pr = do
    t <- Map.mapKeys U.testFilePathFromModuleFilePath <$> build buildTest pr
    return $ Map.insert U.specPath (buildSpec $ Map.keys t) t
  where
    buildSpec :: [FilePath] -> HsModule'
    buildSpec paths =
        module'
            Nothing
            Nothing
            (qualified' . import' . fromString <$>
             imports <> ["Test.QuickCheck", "System.Process", "System.Time.Extra", "Data.TransportTypes.FFI"])
            [mainSig, mainDef]
      where
        imports = U.prefixToModuleName . U.pathToPrefix <$> paths
        main = "main"
        mainSig = typeSig main (var "IO" @@ var "()")
        mainDef = funBind main $ match [] mainBdy
          where
            testName = "prop_encdecInv"
            mainBdy = do' $ stmt (var "Data.TransportTypes.FFI.start_python") : testCalls ++ [stmt $ var "Data.TransportTypes.FFI.end_python"]
              where
                testCalls =
                    (\t ->
                         stmt $
                         var "Test.QuickCheck.quickCheck" @@
                         var (fromString $ t ++ "." ++ testName)) <$>
                    imports

buildTest :: Payload -> U.ModulePrefix -> HsModule'
buildTest Payload {..} prefix =
    let tgtModuleName = U.prefixToModuleName prefix
        testModuleName = U.testNameFromModuleName tgtModuleName
        extImports =
            U.testNameFromModuleName . U.prefixToModuleName . U.pathToPrefix <$>
            Set.toList externalDeps
        locals = U.testNameFromModuleName <$> gatherLocalImports prefix typeRep
        imports =
            qualified' . import' . fromString <$>
            [ tgtModuleName
            , "Test.QuickCheck"
            , "Test.QuickCheck.Instances"
            , "Test.QuickCheck.Monadic"
            , "Generic.Random"
            , "GHC.Generics"
            , "Data.Yaml"
            , "Data.Aeson"
            , "System.IO"
            , "System.Directory"
            , "Data.Maybe"
            , "Data.ByteString.UTF8"
            , "Data.Text"
            , "Data.ByteString"
            , "Data.ByteString.Lazy"
            , "Data.TransportTypes.FFI"
            , "Control.Exception"
            , "Codec.Binary.UTF8.String"
            ] <>
            extImports <> locals
        exports = Nothing
     in module'
            (Just . fromString $ testModuleName)
            exports
            imports
            [arbitraryInstanceDecl, testSig, test, rawSchemeLitSig, rawSchemeLit]
  where
    qualTypename :: TR.TypeName
    qualTypename = U.prefixToQualTypeName prefix title
    arbitraryInstanceDecl =
        instance'
            (var "Test.QuickCheck.Arbitrary" @@ (var . fromString $ qualTypename))
            [funBind "arbitrary" $ match [] (var "Generic.Random.genericArbitraryU")]
    propName = "prop_encdecInv"
    testSig =
        typeSig propName $ var (fromString qualTypename) --> var "Test.QuickCheck.Property"
    sampleName = "sample"
    test = funBind propName $ match [bvar (fromString sampleName)] testBdy
      where
        testBdy =
            var "Test.QuickCheck.Monadic.monadicIO" @@
            do'
                [ bvar "recScheme" <-- var "Test.QuickCheck.Monadic.run" @@
                  decodeEncode "scheme" (var "rawScheme")
                , bvar "recSample" <-- var "Test.QuickCheck.Monadic.run" @@
                  decodeEncode "object" (var "Data.Yaml.encode" @@ var (fromString sampleName))
                , bvar (fromString resName) <-- var "Test.QuickCheck.Monadic.run" @@
                  (var "Data.TransportTypes.FFI.validateJSON" @@ var "recSample" @@
                   var "recScheme")
                , stmt $
                  case'
                      (var (fromString resName))
                      [ match [bvar "True"] $ var "return" @@ unit
                      , match [bvar "False"] $
                        do'
                            [ stmt $
                              var "Test.QuickCheck.Monadic.run" @@
                              (var "putStrLn" @@ string ("Failed test for " ++ qualTypename))
                            , stmt $
                              var "Test.QuickCheck.Monadic.run" @@
                              (var "putStrLn" @@ string "sample:")
                            , stmt $
                              var "Test.QuickCheck.Monadic.run" @@
                              (var "putStrLn" @@ (var "show" @@ var (fromString sampleName)))
                            , stmt $
                              var "Test.QuickCheck.Monadic.run" @@
                              (var "putStrLn" @@ string "recoded sample:")
                            , stmt $
                              var "Test.QuickCheck.Monadic.run" @@
                              (var "putStrLn" @@ (var "show" @@ var "recSample"))
                            , stmt $
                              var "Test.QuickCheck.Monadic.run" @@
                              (var "putStrLn" @@ string "recoded scheme:")
                            , stmt $
                              var "Test.QuickCheck.Monadic.run" @@
                              (var "putStrLn" @@
                               (var "Codec.Binary.UTF8.String.decode" @@
                                (var "Data.ByteString.unpack" @@ var "recScheme")))
                            ]
                      ]
                , assertTrue
                ]
          where
            resName = "res"
            decodeEncode msg x =
                flip
                    case'
                    [ match [conP "Left" [bvar "x"]] $
                      do'
                          [ stmt $
                            var "putStrLn" @@
                            string ("exception from yaml decoder (" ++ msg ++ ") :")
                          , stmt $ var "print" @@ var "x"
                          , stmt $ var "putStrLn" @@ string "on encoding:"
                          , stmt $ var "print" @@ x
                          , stmt $ var "Control.Exception.throw" @@ var "x"
                          ]
                    , match [conP "Right" [bvar "x"]] $ var "return" @@ var "x"
                    ] $
                do'
                    [ bvar (fromString decodedName) <-- var "Data.Yaml.decodeEither'" @@ x
                    , stmt $
                      var "return" @@
                      (var "Data.ByteString.Lazy.toStrict" @@
                       (var "Data.Aeson.encode" @@
                        (var (fromString decodedName) @::@ var "Data.Yaml.Value")))
                    ]
              where
                decodedName = "decoded"
            assertTrue =
                stmt $ var "Test.QuickCheck.Monadic.assert" @@ var (fromString resName)
    rawSchemeLitName = "rawScheme"
    rawSchemeLitSig = typeSig rawSchemeLitName $ var "Data.ByteString.ByteString"
    rawSchemeLit =
        funBind rawSchemeLitName $
        match [] (var "Data.ByteString.UTF8.fromString" @@ string (T.unpack json))

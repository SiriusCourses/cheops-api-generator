{-# LANGUAGE RecordWildCards #-}

module Data.TransportTypes.CodeGen.TestGen where

import qualified Data.Set    as Set
import           Data.String (fromString)
import qualified Data.Text   as T

import GHC.SourceGen (App ((@@)), BVar (bvar), HasList (list), HsDecl', HsModule', Var (var),
                      case', conP, do', funBind, import', instance', int, lambda, match,
                      matchGRHSs, module', op, qualified', recordUpd, rhs, stmt, string,
                      typeSig, valBind, where', (-->), (<--))

import           Data.List                               (intersperse)
import           Data.TransportTypes.CodeGen.Hylo        (Payload (..))
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import           Data.TransportTypes.CodeGen.TypeGen     (gatherLocalImports)
import qualified Data.TransportTypes.TypeRep             as TR

testName :: String
testName = "prop_encdecInv"

antherTestName :: String
antherTestName = "prop_decencInv"

buildSpec :: [FilePath] -> HsModule'
buildSpec paths =
    module'
        Nothing
        Nothing
        (qualified' . import' . fromString <$> imports <> U.specImports)
        [argsSig, args, mainSig, mainDef]
  where
    imports = U.prefixToModuleName . U.pathToPrefix <$> paths
    argsSig = typeSig "args" (var "Test.QuickCheck.Args")
    args =
        funBind "args" $
        match
            []
            (recordUpd
                 (var "Test.QuickCheck.stdArgs")
                 [ ("Test.QuickCheck.chatty", var "False")
                 , ("Test.QuickCheck.maxSuccess", int 25)
                 , ("Test.QuickCheck.maxSize", int 50)
                 ])
    main = "main"
    mainSig = typeSig main (var "IO" @@ var "()")
    mainDef = funBind main $ match [] mainBdy
      where
        mainBdy =
            do' $
            stmt (var "FFI.start_python") :
            (bvar "pb" <-- progressBar) : testCalls ++ [stmt $ var "FFI.end_python"]
          where
            testCalls =
                intersperse (stmt $ var "System.ProgressBar.incProgress" @@ var "pb" @@ int 1) $
                (\t ->
                     stmt $
                     do'
                         [ stmt $
                           var "Test.QuickCheck.quickCheckWith" @@ var "args" @@
                           var (fromString $ t ++ "." ++ testName)
                         , stmt $
                           var "Test.QuickCheck.quickCheckWith" @@ var "args" @@
                           var (fromString $ t ++ "." ++ antherTestName)
                         ]) <$>
                imports
            progressBar =
                var "System.ProgressBar.newProgressBar" @@ var "System.ProgressBar.defStyle" @@
                int 10 @@
                (var "System.ProgressBar.Progress" @@ int 0 @@
                 int (fromIntegral $ length paths) @@
                 var "()")

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
            tgtModuleName : U.perTestImports <> extImports <> locals
        exports = Nothing
     in module'
            (Just . fromString $ testModuleName)
            exports
            imports
            [ arbitraryInstanceDecl
            , testSig
            , test
            , antoherTestSig
            , antoherTest
            , rawschemaLitSig
            , rawschemaLit
            ]
  where
    qualTypename :: TR.TypeName
    qualTypename = U.prefixToQualTypeName prefix title
    arbitraryInstanceDecl =
        instance'
            (var "Test.QuickCheck.Arbitrary" @@ (var . fromString $ qualTypename))
            [funBind "arbitrary" $ match [] (var "Generic.Random.genericArbitraryU")]
    (testSig, test) = buildtoJSONInvTest qualTypename typeRep
    (antoherTestSig, antoherTest) = buildFromJSONInvTest qualTypename typeRep
    (rawschemaLitSig, rawschemaLit) = buildschemaLiteral json

buildtoJSONInvTest :: TR.TypeName -> TR.TypeRep -> (HsDecl', HsDecl')
buildtoJSONInvTest qualTypename typeRep = (testSig, test)
  where
    propName = antherTestName
    testSig =
        typeSig (fromString propName) $
        var (fromString qualTypename) --> var "Test.QuickCheck.Property"
    test = funBind (fromString propName) $ matchGRHSs [] $ rhs testBdy `where'` [precondition]
      where
        testBdy =
            var "Prototypes.prop_toJSONInv_prot" @@ string qualTypename @@ var "precondition" @@
            var "rawSchema"
        precondition =
            valBind "precondition" $
            case typeRep of
                TR.AnyOfType set' ->
                    let bindNames = (\n -> "part" ++ show n) <$> [1 .. Set.size set']
                        patternMatch =
                            conP (fromString qualTypename) $ bvar . fromString <$> bindNames
                     in lambda [bvar "x"] $
                        case'
                            (var "x")
                            [ match [patternMatch] $
                              var "Prelude.any" @@ var "Prelude.id" @@
                              list
                                  ((var "Data.Maybe.isJust" @@) . var . fromString <$>
                                   bindNames)
                            ]
                _ -> var "Prelude.const" @@ var "Prelude.True"

buildFromJSONInvTest :: TR.TypeName -> TR.TypeRep -> (HsDecl', HsDecl')
buildFromJSONInvTest qualTypename _ = (testSig, test)
  where
    propName = testName
    testSig =
        typeSig (fromString propName) $ var (fromString qualTypename) --> var "Prelude.Bool"
    test = funBind (fromString propName) $ match [] testBdy
      where
        testBdy = var "Prototypes.prop_fromJSONInv_prot"

buildschemaLiteral :: T.Text -> (HsDecl', HsDecl')
buildschemaLiteral json = (rawschemaLitSig, rawschemaLit)
  where
    rawschemaLitName = "rawSchema"
    rawschemaLitSig = typeSig rawschemaLitName $ var "Data.ByteString.ByteString"
    rawschemaLit =
        funBind rawschemaLitName $
        matchGRHSs [] $
        rhs
            (var "Data.ByteString.UTF8.fromString" @@
             (var "replaceOneOf" @@ string (T.unpack json))) `where'`
        [ valBind "replaceOneOf" $
          foldr1
              (`op` ".")
              [ var "Data.Text.unpack"
              , var "Data.Text.replace" @@ string "oneOf:" @@ string "anyOf:"
              , var "Data.Text.pack"
              ]
        ]
{-
var "Test.QuickCheck.Monadic.monadicIO" @@
            do'
                [ (strictP . bvar . fromString $ recSchemaName) <--
                  var "Test.QuickCheck.Monadic.run" @@
                  yaml2jsonSchema
                , tuple [bvar "res", bvar "genSample"] <-- var "Test.QuickCheck.Monadic.run" @@
                  (var "FFI.withGeneratedBySchema" @@ var (fromString recSchemaName) @@
                   lambda [bvar "rawSample"] insepectSample)
                , analyzeResult
                , stmt $ var "Test.QuickCheck.Monadic.assert" @@ var "res"
                ]
          where
            recSchemaName :: String
            recSchemaName = "recSchema"
            yaml2jsonSchema = decodeEncode "schema" (var "rawSchema")
            insepectSample =
                case'
                    json2yaml2object
                    [ match [conP "Prelude.Left" [bvar "x"]] errorInspection
                    , match
                          [ conP
                                "Prelude.Right"
                                [sigP (bvar "x") (var . fromString $ qualTypename)]
                          ]
                          (do' [ bvar "res" <-- validateObject
                               , stmt $ var "Prelude.return" @@ tuple [var "res", var "x"]
                               ])
                    ]
              where
                jsonToValidate =
                    decodeEncode
                        "recoding data to yaml and to json to validate"
                        (var "Data.Yaml.encode" @@ var "x")
                validateObject =
                    op
                        (lambda [bvar "yamlObj"] $
                         var "FFI.validateJSON" @@ var "yamlObj" @@ var (fromString recSchemaName))
                        "=<<"
                        jsonToValidate
                errorInspection =
                    do'
                        [ stmt $
                          var "Prelude.putStrLn" @@
                          string ("Exception during fromJSON test of " ++ qualTypename)
                        , stmt $
                          var "Prelude.putStrLn" @@
                          string "exception while recoding generated object:"
                        , stmt $ var "Prelude.print" @@ var "x"
                        , stmt $ var "Prelude.putStrLn" @@ string "sample in text form:"
                        , stmt $ var "Prelude.print" @@ var "rawSample"
                        , stmt $ var "Prelude.putStrLn" @@ string "schema:"
                        , stmt $
                          var "Prelude.putStrLn" @@
                          (var "Codec.Binary.UTF8.String.decode" @@
                           (var "Data.ByteString.unpack" @@ var (fromString recSchemaName)))
                        , stmt $ var "fail" @@ var "x"
                        ]
                json2yaml2object =
                    do'
                        [ bvar "decoded" <-- var "Data.Aeson.eitherDecodeStrict'" @@
                          var "rawSample"
                        , bvar "recSample" <--
                          (var "Prelude.return" @@
                           (var "Data.Yaml.encode" @@
                            (var "decoded" GCH.SG.@::@ var "Data.Aeson.Value")))
                        , stmt $
                          var "Prelude.either" @@
                          op (var "Prelude.Left") "." (var "Prelude.show") @@
                          var "Prelude.Right" @@
                          (var "Data.Yaml.decodeEither'" @@ var "recSample")
                        ]
            analyzeResult =
                stmt $
                var "Control.Monad.unless" @@ var "res" @@
                (var "Test.QuickCheck.Monadic.run" @@
                 do'
                     [ stmt $ var "Prelude.putStrLn" @@ string ""
                     , stmt $ var "Prelude.putStrLn" @@ string ""
                     , stmt $
                       var "Prelude.putStrLn" @@
                       string ("Failed test(fromJSON) for " ++ qualTypename)
                     , stmt $ var "Prelude.putStrLn" @@ string "sample:"
                     , stmt $ var "Prelude.print" @@ var "genSample"
                     , stmt $ var "Prelude.putStrLn" @@ string "encoded sample:"
                     , stmt $
                       var "Prelude.putStrLn" @@
                       (var "Codec.Binary.UTF8.String.decode" @@
                        (var "Data.ByteString.unpack" @@
                         (var "Data.Yaml.encode" @@ var "genSample")))
                     , stmt $ var "Prelude.putStrLn" @@ string "recoded schema:"
                     , stmt $
                       var "Prelude.putStrLn" @@
                       (var "Codec.Binary.UTF8.String.decode" @@
                        (var "Data.ByteString.unpack" @@ var (fromString recSchemaName)))
                     ])




let precondition =
                    case typeRep of
                        TR.AnyOfType set' ->
                            let bindNames = (\n -> "part" ++ show n) <$> [1 .. Set.size set']
                                patternMatch =
                                    conP (fromString qualTypename) $
                                    bvar . fromString <$> bindNames
                             in case'
                                    (var "sample'")
                                    [ match [patternMatch] $
                                      var "Prelude.any" @@ var "Prelude.id" @@
                                      list
                                          ((var "Data.Maybe.isJust" @@) . var . fromString <$>
                                           bindNames)
                                    ]
                        _ -> var "Prelude.True"
             in var "Test.QuickCheck.Monadic.monadicIO" @@
                do'
                    [ stmt $ var "Test.QuickCheck.Monadic.pre" @@ precondition
                    , strictP (bvar (fromString resName)) <-- var "Test.QuickCheck.Monadic.run" @@
                      do'
                          [ strictP (bvar "recSchema") <--
                            decodeEncode "schema" (var "rawSchema")
                          , bvar (fromString sampleName) <--
                            case' (var "sample'") [match [bvar "s"] $ var "pure" @@ var "s"]
                          , strictP (bvar "recSample") <--
                            decodeEncode
                                "object"
                                (var "Data.Yaml.encode" @@ var (fromString sampleName))
                          , stmt $ var "FFI.validateJSON" @@ var "recSample" @@ var "recSchema"
                          ]
                    , stmt $
                      var "Control.Monad.unless" @@ var "res" @@
                      (var "Test.QuickCheck.Monadic.run" @@
                       do'
                           [ stmt $ var "putStrLn" @@ string ""
                           , stmt $ var "putStrLn" @@ string ""
                           , stmt $
                             var "putStrLn" @@
                             string ("Failed test(toJSON) for " ++ qualTypename)
                           , stmt $ var "putStrLn" @@ string "sample:"
                           , stmt $ var "print" @@ var "sample'"
                           , stmt $ var "putStrLn" @@ string "recoded schema:"
                           , stmt $
                             var "putStrLn" @@
                             (var "Codec.Binary.UTF8.String.decode" @@
                              (var "Data.ByteString.unpack" @@ var "rawSchema"))
                           ])
                    , assertTrue
                    ]
          where
            resName = "res"
            assertTrue =
                stmt $ var "Test.QuickCheck.Monadic.assert" @@ var (fromString resName)



decodeEncode :: String -> HsExpr' -> HsExpr'
decodeEncode msg x =
    flip
        case'
        [ match [conP "Left" [bvar "x'"]] $
          do'
              [ stmt $
                var "putStrLn" @@ string ("exception from yaml decoder (" ++ msg ++ ") :")
              , stmt $ var "print" @@ var "x'"
              , stmt $ var "putStrLn" @@ string "on encoding:"
              , stmt $ var "print" @@ x
              , stmt $ var "Control.Exception.throw" @@ var "x'"
              ]
        , match [conP "Right" [bvar "x'"]] $ var "return" @@ var "x'"
        ] $
    do' [ sigP (bvar (fromString decodedName)) (var "Data.Yaml.Value") <-- var "Prelude.fmap" @@
          var "Data.TransportTypes.Utils.addEmptyPropertiesToObjects" @@
          (var "Data.Yaml.decodeEither'" @@ x)
        , stmt $
          var "return" @@
          (var "Data.ByteString.Lazy.toStrict" @@
           (var "Data.Aeson.encode" @@ var (fromString decodedName)))
        ]
  where
    decodedName = "decoded"

-}

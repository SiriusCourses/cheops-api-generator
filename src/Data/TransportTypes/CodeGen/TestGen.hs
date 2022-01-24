{-# LANGUAGE RecordWildCards #-}

module Data.TransportTypes.CodeGen.TestGen where

import qualified Data.Set    as Set
import           Data.String (fromString)
import qualified Data.Text   as T

import           GHC.SourceGen (App ((@@)), BVar (bvar), HasTuple (unit), HsModule', Var (var),
                                case', conP, do', funBind, import', instance', int, list,
                                match, matchGRHSs, module', op, qualified', recordUpd, rhs,
                                stmt, strictP, string, typeSig, valBind, where', (-->), (<--))
import qualified GHC.SourceGen as GCH.SG

import           Data.List                               (intersperse)
import           Data.TransportTypes.CodeGen.Hylo        (Payload (..))
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import           Data.TransportTypes.CodeGen.TypeGen     (gatherLocalImports)
import qualified Data.TransportTypes.TypeRep             as TR

buildSpec :: [FilePath] -> HsModule'
buildSpec paths =
    module'
        Nothing
        Nothing
        (qualified' . import' . fromString <$> imports <> U.specImports)
        [mainSig, mainDef]
  where
    imports = U.prefixToModuleName . U.pathToPrefix <$> paths
    main = "main"
    mainSig = typeSig main (var "IO" @@ var "()")
    mainDef = funBind main $ match [] mainBdy
      where
        testName = "prop_encdecInv"
        mainBdy =
            do' $
            stmt (var "FFI.start_python") :
            (bvar "pb" <-- progressBar) : testCalls ++ [stmt $ var "FFI.end_python"]
          where
            testCalls =
                intersperse (stmt $ var "System.ProgressBar.incProgress" @@ var "pb" @@ int 1) $
                (\t ->
                     stmt $
                     var "Test.QuickCheck.quickCheckWith" @@
                     recordUpd
                         (var "Test.QuickCheck.stdArgs")
                         [ ("Test.QuickCheck.chatty", var "False")
                         , ("Test.QuickCheck.maxSuccess", int 25)
                         , ("Test.QuickCheck.maxSize", int 50)
                         ] @@
                     var (fromString $ t ++ "." ++ testName)) <$>
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
    test = funBind propName $ match [bvar "sample'"] testBdy
      where
        testBdy =
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
                    -- , stmt $
                    --   var "Test.QuickCheck.Monadic.run" @@
                    --   (var "putStrLn" @@ string (">>> Testing: " ++ qualTypename))
                    -- , stmt $
                    --   var "Test.QuickCheck.Monadic.run" @@
                    --   (var "putStrLn" @@ string "getting scheme")
                    , strictP (bvar "recScheme") <-- var "Test.QuickCheck.Monadic.run" @@
                      decodeEncode "scheme" (var "rawScheme")
                    -- , stmt $
                    --   var "Test.QuickCheck.Monadic.run" @@
                    --   (var "putStrLn" @@ string "sampling sample")
                    , bvar (fromString sampleName) <-- var "Test.QuickCheck.Monadic.run" @@
                      case' (var "sample'") [match [bvar "s"] $ var "pure" @@ var "s"]
                    -- , stmt $
                    --   var "Test.QuickCheck.Monadic.run" @@
                    --   (var "putStrLn" @@ string "encoding-decoding sample")
                    , strictP (bvar "recSample") <-- var "Test.QuickCheck.Monadic.run" @@
                      decodeEncode
                          "object"
                          (var "Data.Yaml.encode" @@ var (fromString sampleName))
                    -- , stmt $
                    --   var "Test.QuickCheck.Monadic.run" @@
                    --   (var "putStrLn" @@ string "validating")
                    , strictP (bvar (fromString resName)) <-- var "Test.QuickCheck.Monadic.run" @@
                      (var "FFI.validateJSON" @@ var "recSample" @@ var "recScheme")
                    -- , stmt $
                    --   var "Test.QuickCheck.Monadic.run" @@
                    --   (var "putStrLn" @@ string "inspecting result")
                    , stmt $
                      case'
                          (var (fromString resName))
                          [ match [bvar "True"] $ var "return" @@ unit
                          , match [bvar "False"] $
                            var "Test.QuickCheck.Monadic.run" @@
                            do'
                                [ stmt $ var "putStrLn" @@ string ""
                                , stmt $ var "putStrLn" @@ string ""
                                , stmt $
                                  var "putStrLn" @@ string ("Failed test for " ++ qualTypename)
                                , stmt $ var "putStrLn" @@ string "sample:"
                                , stmt $
                                  var "putStrLn" @@ (var "show" @@ var (fromString sampleName))
                                , stmt $ var "putStrLn" @@ string "recoded sample:"
                                , stmt $ var "putStrLn" @@ (var "show" @@ var "recSample")
                                , stmt $ var "putStrLn" @@ string "recoded scheme:"
                                , stmt $
                                  var "putStrLn" @@
                                  (var "Codec.Binary.UTF8.String.decode" @@
                                   (var "Data.ByteString.unpack" @@ var "recScheme"))
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
                        (var (fromString decodedName) GCH.SG.@::@ var "Data.Yaml.Value")))
                    ]
              where
                decodedName = "decoded"
            assertTrue =
                stmt $ var "Test.QuickCheck.Monadic.assert" @@ var (fromString resName)
    rawSchemeLitName = "rawScheme"
    rawSchemeLitSig = typeSig rawSchemeLitName $ var "Data.ByteString.ByteString"
    rawSchemeLit =
        funBind rawSchemeLitName $
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

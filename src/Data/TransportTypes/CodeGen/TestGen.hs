{-# LANGUAGE RecordWildCards #-}

module Data.TransportTypes.CodeGen.TestGen where

import qualified Data.Set    as Set
import           Data.String (fromString)
import qualified Data.Text   as T

import GHC.SourceGen (App ((@@)), BVar (bvar), HasList (list), HsDecl', HsModule', Var (var),
                      case', conP, do', funBind, import', instance', int, lambda, match,
                      matchGRHSs, module', op, qualified', recordUpd, rhs, stmt, string, tyApp,
                      typeSig, valBind, where', (-->), (<--))

import           Data.List                               (intersperse)
import           Data.TransportTypes.CodeGen.Hylo        (Payload (..))
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import           Data.TransportTypes.CodeGen.TypeGen     (gatherLocalImports)
import qualified Data.TransportTypes.TypeRep             as TR

testName :: String
testName = "prop_validationTest"

antherTestName :: String
antherTestName = "prop_decencTest"

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
            testCalls
                --intersperse (stmt $ var "System.ProgressBar.incProgress" @@ var "pb" @@ int 1) $
             =
                (\t ->
                     stmt $
                     do'
                        --    stmt $
                        --    var "Test.QuickCheck.quickCheckWith" @@ var "args" @@
                        --    var (fromString $ t ++ "." ++ testName)
                        --  ,
                         [ stmt $
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
    (testSig, test) = buildtoValidationTest qualTypename typeRep
    (antoherTestSig, antoherTest) = buildEncodingInvariantTest qualTypename typeRep
    (rawschemaLitSig, rawschemaLit) = buildschemaLiteral json

buildtoValidationTest :: TR.TypeName -> TR.TypeRep -> (HsDecl', HsDecl')
buildtoValidationTest qualTypename typeRep = (testSig, test)
  where
    propName = testName
    testSig =
        typeSig (fromString propName) $
        var (fromString qualTypename) --> var "Test.QuickCheck.Property"
    test = funBind (fromString propName) $ matchGRHSs [] $ rhs testBdy `where'` [precondition]
      where
        testBdy =
            var "Prototypes.validationTest_prototype" @@ string qualTypename @@
            var "precondition" @@
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
                              var "Prelude.any" `tyApp` var "[]" @@ var "Prelude.id" @@
                              list
                                  ((var "Data.Maybe.isJust" @@) . var . fromString <$>
                                   bindNames)
                            ]
                _ -> var "Prelude.const" @@ var "Prelude.True"

buildEncodingInvariantTest :: TR.TypeName -> TR.TypeRep -> (HsDecl', HsDecl')
buildEncodingInvariantTest qualTypename _ = (testSig, test)
  where
    propName = antherTestName
    testSig =
        typeSig (fromString propName) $ var (fromString qualTypename) --> var "Prelude.Bool"
    test = funBind (fromString propName) $ match [] testBdy
      where
        testBdy = var "Prototypes.encodingDecodingInvariantTest_prototype"

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

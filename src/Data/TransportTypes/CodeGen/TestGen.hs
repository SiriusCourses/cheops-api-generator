{-# LANGUAGE RecordWildCards #-}

module Data.TransportTypes.CodeGen.TestGen where

import qualified Data.Set    as Set
import           Data.String (fromString)
import qualified Data.Text   as T

import           GHC.SourceGen (App ((@@)), BVar (bvar), HasTuple (unit), HsModule', Var (var),
                                case', conP, do', funBind, import', instance', match, module',
                                qualified', stmt, string, typeSig, (-->), (<--))
import qualified GHC.SourceGen as GCH.SG

import           Data.TransportTypes.CodeGen.Hylo        (Payload (..))
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import           Data.TransportTypes.CodeGen.TypeGen     (gatherLocalImports)
import qualified Data.TransportTypes.TypeRep             as TR

perTestImports :: [String]
perTestImports =
    [ "Test.QuickCheck"
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
    ]

specImports :: [String]
specImports = ["Test.QuickCheck", "Data.TransportTypes.FFI"]

buildSpec :: [FilePath] -> HsModule'
buildSpec paths =
    module'
        Nothing
        Nothing
        (qualified' . import' . fromString <$> imports <> specImports)
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
            stmt (var "Data.TransportTypes.FFI.start_python") :
            testCalls ++ [stmt $ var "Data.TransportTypes.FFI.end_python"]
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
            tgtModuleName : perTestImports <> extImports <> locals
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
        match [] (var "Data.ByteString.UTF8.fromString" @@ string (T.unpack json))

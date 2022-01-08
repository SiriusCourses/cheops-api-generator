{-# LANGUAGE RecordWildCards #-}

module Data.TransportTypes.CodeGen
    ( buildTests
    , buildModules
    ) where

import Control.Monad ((<=<))
import Data.Foldable (toList)

import           Data.Bifunctor  (bimap)
import           Data.Foldable   (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes, mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.String     (fromString)

import GHC.SourceGen

import qualified Data.Text                               as T
import           Data.TransportTypes.CodeGen.Hylo        (Payload (..), build)
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import           Data.TransportTypes.Parsing             (ParserResult (..))
import qualified Data.TransportTypes.TypeRep             as TR

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
    gatherSum = (catMaybes . snd) <=< Map.toList . fmap TR.unSumConstr . Map.map (fmap go)
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
            (extImports <> locals <> defaultImports <> specialDerivImports)
            [buildTypeDecl typeRep]
  where
    typename :: TR.TypeName
    typename = U.prefixToTypeName prefix title
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
             imports <> ["Test.QuickCheck", "System.Process", "System.Time.Extra"])
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
                [ bvar (fromString handleName) <-- startTestServer
                , stmt $ var "System.Time.Extra.sleep" @@ int 1
                ] ++
                (testCalls) ++ [stopTestServer]
              where
                handleName = "handle"
                startTestServer =
                    var "System.Process.spawnProcess" @@ string "sh" @@
                    (var "pure" @@ string "python/JsonTest/run.sh")
                stopTestServer =
                    stmt $ var "System.Process.terminateProcess" @@ var (fromString handleName)
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
            jsonBinding $
            var "Test.QuickCheck.Monadic.monadicIO" @@
            do'
                [ writeJsonsToFIFO
                , bvar (fromString resName) <-- readAnswerFromFIFO
                , assertTrue
                ]
          where
            resName = "res"
            -- testSampling =
            --     bvar (fromString sampleName) <-- var "Test.QuickCheck.Monadic.pick" @@
            --     (var "Test.QuickCheck.arbitrary" `tyApp` var (fromString qualTypename))
            jsonBinding =
                let converting x =
                        var "Data.ByteString.Lazy.toStrict" @@
                        (var "Data.Aeson.encode" @@
                         (var "Data.Maybe.fromJust" @@
                          ((var "Data.Yaml.decode" `tyApp` var "Data.Yaml.Object") @@ x)))
                 in let'
                        [ valBind "jsonScheme" $ converting (var "rawScheme")
                        , funBind "jsonObject" $
                          match [bvar "x"] $ converting (var "Data.Yaml.encode" @@ var "x")
                        ]
            writeJsonsToFIFO =
                let mod' = var "System.IO.WriteMode"
                    file = string "test_input.fifo"
                 in stmt $
                    var "Test.QuickCheck.Monadic.run" @@
                    (var "System.IO.withFile" @@ file @@ mod' @@ writing)
              where
                writing =
                    lambda [bvar "handle"] $
                    do'
                        [ stmt $
                          var "Data.ByteString.hPut" @@ var "handle" @@
                          (var "jsonObject" @@ var (fromString sampleName))
                        , stmt $ var "System.IO.hPutStrLn" @@ var "handle" @@ string ""
                        , stmt $ var "Data.ByteString.hPut" @@ var "handle" @@ var "jsonScheme"
                        ]
            readAnswerFromFIFO =
                let mod' = var "System.IO.ReadMode"
                    file = string "test_output.fifo"
                 in var "Test.QuickCheck.Monadic.run" @@
                    (var "System.IO.withFile" @@ file @@ mod' @@ reading)
              where
                reading = var "System.IO.hGetLine"
            assertTrue =
                stmt $
                var "Test.QuickCheck.Monadic.assert" @@
                (var "read" @@ (var (fromString resName)))
    rawSchemeLitName = "rawScheme"
    rawSchemeLitSig = typeSig rawSchemeLitName $ var "Data.ByteString.ByteString"
    rawSchemeLit =
        funBind rawSchemeLitName $
        match [] (var "Data.ByteString.UTF8.fromString" @@ string (T.unpack json))
    -- retTH = var "return" @@ var "[]"
    -- testcallsTH = funBind "runTests" $ match [] (var "$Test.QuickCheck.quickCheckAll")
{-prop_encdecInv :: Test.QuickCheck.Monadic.Property
  prop_encdecInv =
      Test.QuickCheck.Monadic.monadicIO $ do
          sample <- Test.QuickCheck.Monadic.pick (Test.QuickCheck.arbitrary @Cheops.Transport.Bug.V0.Api.Api)
          let jsonScheme =
                  Data.Aeson.encode . fromJust . (Data.Yaml.decode @Data.Yaml.Object) $ rawScheme
          let jsonObject =
                  Data.Aeson.encode . fromJust . (Data.Yaml.decode @Data.Yaml.Object) $
                  Data.Yaml.encode sample
          Test.QuickCheck.Monadic.run .
              System.IO.withFile System.IO.WriteMod "python/JsonTest/test_input.fifo" $ do
              System.IO.hPutStrLn jsonObject
              System.IO.hPutStrLn jsonScheme
          [res] <-
              Test.QuickCheck.Monadic.run .
              System.IO.withFile
                  System.IO.ReadMod
                  "python/JsonTest/test_ouput.fifo"
                  System.IO.hGetLine
          Test.QuickCheck.Monadic.assert $ read res
-}

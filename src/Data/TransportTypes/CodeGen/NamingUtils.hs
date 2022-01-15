{-|
Module      : Data.TransportTypes.CodeGen.NamingUtils
Description : Helper functions for getting type names and module names from title and prefix
-}
module Data.TransportTypes.CodeGen.NamingUtils where

import           Data.Function               ((&))
import           Data.List                   (intersperse)
import           Data.Maybe                  (fromMaybe)

import qualified Data.TransportTypes.TypeRep as TR
          
import           System.FilePath             (dropExtension, replaceFileName, takeBaseName,
                                              (<.>), (</>))
import           Text.Casing                 (pascal)
import           Util                        (capitalise, split)

type ModulePrefix = [String]

type Title = String

type ModuleName = String

type QualTypeName = String

type FieldName = String

-- | Constant to use in an absence of name. For example if prefix is empty and module has no title
unnamed :: String
unnamed = "Unnamed"

-- | Appends capitalized name as head to prefix
updatePrefix :: FieldName -> ModulePrefix -> ModulePrefix
updatePrefix fieldName prefix = capitalise fieldName : prefix

-- | Converts path to haskell module to qualified module name
pathToPrefix :: FilePath -> ModulePrefix
pathToPrefix []   = [unnamed]
pathToPrefix path = reverse . fmap capitalise . split '/' $ path & dropExtension

-- | Converts prefix to qualified module name
prefixToModuleName :: ModulePrefix -> ModuleName
prefixToModuleName prefix = mconcat . reverse . intersperse "." $ capitalise <$> prefix

-- | Converts prefix and title to qualified type name which is exported by correspondnig module
prefixToQualTypeName :: ModulePrefix -> Maybe Title -> QualTypeName
prefixToQualTypeName p@(s:_) tn =
    mconcat . reverse . intersperse "." $ capitalise <$> fromMaybe s tn : p
prefixToQualTypeName [] (Just tn) = unnamed ++ "." ++ capitalise tn
prefixToQualTypeName [] Nothing = unnamed ++ "." ++ unnamed

-- | Converts prefix and title to non-qualified type name which is exported by corresponding module
prefixToTypeName :: ModulePrefix -> Maybe Title -> TR.TypeName
prefixToTypeName (s:_) tn = capitalise $ fromMaybe s tn
prefixToTypeName [] tn    = capitalise $ fromMaybe unnamed tn

-- | Converts prefix to path to haskell module
prefixToPath :: ModulePrefix -> FilePath
prefixToPath prefix = foldl1 (</>) (reverse prefix) <.> "hs"

-- | Converts type reference and prefix to qualified type name exported be corresponding module
referenceToQualTypeName :: ModulePrefix -> TR.TypeRef -> QualTypeName
referenceToQualTypeName _ (TR.ReferenceToExternalType absPath tn) =
    prefixToQualTypeName (pathToPrefix absPath) (Just $ capitalise tn)
referenceToQualTypeName _ (TR.ReferenceToPrimitiveType s) = s
referenceToQualTypeName prefix (TR.ReferenceToLocalType fieldName tn) =
    prefixToQualTypeName (fieldName : prefix) (Just $ capitalise tn)

-- | Converts type reference as in 'Data.TransportTypes.TypeRep' to non-qualified type name exported be corresponding module
referenceToTypeName :: TR.TypeRef -> TR.TypeName
referenceToTypeName (TR.ReferenceToExternalType _ tn) = capitalise tn
referenceToTypeName (TR.ReferenceToPrimitiveType s)   = s
referenceToTypeName (TR.ReferenceToLocalType _ tn)    = capitalise tn

-- | Converts type reference as in 'Data.TransportTypes.TypeRep' to qualified module name. Returns 'Nothing' in case of primitive type
referenceToModuleName :: ModulePrefix -> TR.TypeRef -> Maybe ModuleName
referenceToModuleName _ (TR.ExtRef tr) = nonLocalReferenceToModuleName tr
referenceToModuleName prefix (TR.LocRef (TR.LocalReference s _)) =
    Just $ mconcat . reverse . intersperse "." $ capitalise s : prefix

-- | Converts non-local type reference as in 'Data.TransportTypes.TypeRep' to qualified module name. Returns 'Nothing' in case of primitive type
nonLocalReferenceToModuleName :: TR.NonLocalRef -> Maybe ModuleName
nonLocalReferenceToModuleName (TR.RefPrimitiveType _) = Nothing
nonLocalReferenceToModuleName (TR.RefExternalType absPath _) =
    Just . prefixToModuleName . pathToPrefix $ absPath

-- | Converts non-local type reference as in 'Data.TransportTypes.TypeRep' to qualified type name exproted be corresponding module. Returns 'Nothing' in case of primitive type
nonLocalReferenceToQualTypeName :: TR.NonLocalRef -> QualTypeName
nonLocalReferenceToQualTypeName (TR.RefPrimitiveType s) = s
nonLocalReferenceToQualTypeName (TR.RefExternalType absPath tn) =
    prefixToTypeName (pathToPrefix absPath) (Just $ capitalise tn)

-- | Extracts non-qualified type name from title and path to corresponding module
typeNameFromAbsolutePath :: FilePath -> Maybe Title -> TR.TypeName
typeNameFromAbsolutePath fp Nothing     = takeBaseName (fp & dropExtension)
typeNameFromAbsolutePath _ (Just title) = capitalise title

-- | Translates some of haskell's keywords to something less harmful for compilation process. Usually by prepending "_" and appeinding "\'". If the word is not harmful functions as 'id'
changeReservedNames :: String -> String
changeReservedNames "type"    = "_type'"
changeReservedNames "data"    = "_data'"
changeReservedNames "module"  = "_module'"
changeReservedNames "default" = "_default'"
changeReservedNames x         = x

-- | Reverts haskell's reserved names translation
changeReservedNamesBack :: String -> String
changeReservedNamesBack "_type'"    = "type"
changeReservedNamesBack "_data'"    = "data"
changeReservedNamesBack "_module'"  = "module"
changeReservedNamesBack "_default'" = "default"
changeReservedNamesBack x         = x

-- | Translates string to pascal case.
fieldNameToSumCon :: FieldName -> String
fieldNameToSumCon = pascal

-- | Generates pattern name by appending prime to it
fieldNameToPatName :: FieldName -> String
fieldNameToPatName = (++ "'")

-- | Extracts name from string and title
chooseName :: FieldName -> Maybe Title -> TR.TypeName
chooseName fn m_title = capitalise $ fromMaybe fn m_title

-- | Prepending "un" to typename. Used for getters in newtypes.
getterName :: TR.TypeName -> FieldName
getterName = ("un" ++)

-- | Constant for global prefix
globalPrefix :: FilePath
globalPrefix = "Cheops" </> "Transport"

-- | Constant for makeing test module
testSuffix :: String
testSuffix = "Test"

-- | Constant for path to test main
specPath :: String
specPath = "Spec.hs"

-- | Converts qualified moduele name to corresponding qualified test module name
testNameFromModuleName :: TR.ModuleName -> TR.ModuleName
testNameFromModuleName = (++ testSuffix)

-- | Converts filepath to module to filepath to test of said module
testFilePathFromModuleFilePath :: FilePath -> FilePath
testFilePathFromModuleFilePath fn = replaceFileName fn $ takeBaseName fn ++ "Test" <.> "hs"

-- | Constant for default imports
defaultImportNames :: [String]
defaultImportNames =
    [ "Data.Yaml"
    , "GHC.Generics"
    , "GHC.Types"
    , "GHC.Int"
    , "Data.Text"
    , "Data.Vector"
    , "Data.Scientific"
    , "Data.Maybe"
    , "Data.Bifunctor"
    , "Prelude"
    ]
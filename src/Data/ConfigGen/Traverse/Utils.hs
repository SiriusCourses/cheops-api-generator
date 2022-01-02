module Data.ConfigGen.Traverse.Utils where

import qualified Data.ConfigGen.TypeRep as TR
import           Data.Function          ((&))
import           Data.List              (intersperse)
import           Data.Maybe             (fromMaybe)
import           Data.String            (fromString)
import           GHC.SourceGen
import           System.FilePath        (dropExtension, takeBaseName, (<.>), (</>))
import           Text.Casing            (pascal)
import           Util                   (capitalise, split)

type ModulePrefix = [String]

type Title = String

type ModuleName = String

type QualTypeName = String

type FieldName = String

unnamed :: String
unnamed = "Unnamed"

updatePrefix :: FieldName -> ModulePrefix -> ModulePrefix
updatePrefix fieldName prefix = capitalise fieldName : prefix

pathToPrefix :: FilePath -> ModulePrefix
pathToPrefix []   = [unnamed]
pathToPrefix path = reverse . fmap capitalise . split '/' $ path & dropExtension

prefixToModuleName :: ModulePrefix -> ModuleName
prefixToModuleName prefix = mconcat . reverse . intersperse "." $ capitalise <$> prefix

prefixToQualTypeName :: ModulePrefix -> Maybe Title -> QualTypeName
prefixToQualTypeName p@(s:_) tn =
    mconcat . reverse . intersperse "." $ capitalise <$> fromMaybe s tn : p
prefixToQualTypeName [] (Just tn) = unnamed ++ "." ++ capitalise tn
prefixToQualTypeName [] Nothing = unnamed ++ "." ++ unnamed

prefixToTypeName :: ModulePrefix -> Maybe Title -> TR.TypeName
prefixToTypeName (s:_) tn = capitalise $ fromMaybe s tn
prefixToTypeName [] tn    = capitalise $ fromMaybe unnamed tn

prefixToPath :: ModulePrefix -> FilePath
prefixToPath prefix = foldl1 (</>) (reverse prefix) <.> "hs"

referenceToQualTypeName :: ModulePrefix -> TR.TypeRef -> QualTypeName
referenceToQualTypeName _ (TR.ReferenceToExternalType absPath tn) =
    prefixToQualTypeName (pathToPrefix absPath) (Just $ capitalise tn)
referenceToQualTypeName _ (TR.ReferenceToPrimitiveType s) = s
referenceToQualTypeName prefix (TR.ReferenceToLocalType fieldName tn) =
    prefixToQualTypeName (fieldName : prefix) (Just $ capitalise tn)

referenceToTypeName :: TR.TypeRef -> TR.TypeName
referenceToTypeName (TR.ReferenceToExternalType _ tn) = capitalise tn
referenceToTypeName (TR.ReferenceToPrimitiveType s)   = s
referenceToTypeName (TR.ReferenceToLocalType _ tn)    = capitalise tn

referenceToModuleName :: ModulePrefix -> TR.TypeRef -> Maybe QualTypeName
referenceToModuleName _ (TR.ExtRef tr) = nonLocalReferenceToModuleName tr
referenceToModuleName prefix (TR.LocRef (TR.LocalReference s _)) =
    Just $ mconcat . reverse . intersperse "." $ capitalise s : prefix

nonLocalReferenceToModuleName :: TR.NonLocalRef -> Maybe ModuleName
nonLocalReferenceToModuleName (TR.RefPrimitiveType _) = Nothing
nonLocalReferenceToModuleName (TR.RefExternalType absPath _) =
    Just . prefixToModuleName . pathToPrefix $ absPath

nonLocalReferenceToQualTypeName :: TR.NonLocalRef -> QualTypeName
nonLocalReferenceToQualTypeName (TR.RefPrimitiveType s) = s
nonLocalReferenceToQualTypeName (TR.RefExternalType absPath tn) =
    prefixToTypeName (pathToPrefix absPath) (Just $ capitalise tn)

typeNameFromAbsolutePath :: FilePath -> Maybe Title -> TR.TypeName
typeNameFromAbsolutePath fp Nothing     = takeBaseName (fp & dropExtension)
typeNameFromAbsolutePath _ (Just title) = capitalise title

changeReservedNames :: String -> String
changeReservedNames "type"    = "type'"
changeReservedNames "data"    = "data'"
changeReservedNames "module"  = "_module'"
changeReservedNames "default" = "_default'"
changeReservedNames x         = x

fieldNameToSumCon :: FieldName -> String
fieldNameToSumCon = pascal

chooseName :: FieldName -> Maybe Title -> TR.TypeName
chooseName fn m_title = capitalise $ fromMaybe fn m_title

getterName :: TR.TypeName -> FieldName
getterName = ("un" ++)

globalPrefix :: FilePath
globalPrefix = "Cheops" </> "Transport"

defaultImportNames :: [String]
defaultImportNames =
    [ "Data.Yaml"
    , "GHC.Generics"
    , "GHC.Types"
    , "GHC.Int"
    , "Data.Text"
    , "Data.Vector"
    , "Data.Scientific"
    ]

defaultDerivingClause :: [HsDerivingClause']
defaultDerivingClause = [generic, json]
  where
    generic = deriving' [var "GHC.Generics.Generic"]
    json = derivingAnyclass [var "Data.Yaml.ToJSON", var "Data.Yaml.FromJSON"]

aofDerivingClause :: TR.TypeName -> [HsDerivingClause']
aofDerivingClause typename = [tojson, fromjson, generic]
  where
    generic = deriving' [var "GHC.Generics.Generic"]
    fromjson = derivingAnyclass [var "Data.Yaml.FromJSON"]
    tojson =
        derivingVia
            (var "Data.ConfigGen.Deriv.AOf" @@ (var . fromString $ typename))
            [var "Data.Yaml.ToJSON"] --, var "Data.Yaml.FromJSON"]

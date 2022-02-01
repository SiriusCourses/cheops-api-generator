module Data.TransportTypes.CodeGen.TypeGen.InstanceGen.ToSchema
    ( buildToSchemaInstance
    ) where

import           Data.Aeson                              (encode)
import           Data.Bifunctor                          (second)
import           Data.ByteString.Lazy                    (toStrict)
import qualified Data.Map.Strict                         as Map
import qualified Data.Set                                as Set
import           Data.String                             (IsString (fromString))
import qualified Data.Text                               as T
import           Data.Text.Encoding                      (decodeUtf8)
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import qualified Data.TransportTypes.TypeRep             as TR
import           GHC.SourceGen
import qualified GHC.SourceGen                           as GHC.SG

amp :: HsExpr' -> HsExpr' -> HsExpr'
amp = (`op` "Control.Lens.&")

replaceUnitProxy :: String -> String
replaceUnitProxy "Data.TransportTypes.Utils.UnitProxy" = "()"
replaceUnitProxy x                                     = x

buildToSchemaInstance ::
       Maybe U.Title -> U.ModulePrefix -> U.QualTypeName -> TR.TypeRep -> HsDecl'
buildToSchemaInstance title prefix qualTypename (TR.ProdType map') =
    instance'
        (var "Data.Swagger.ToSchema" @@ var (fromString qualTypename))
        [funBind "declareNamedSchema" $ match [bvar "_proxy"] bdy]
  where
    binds =
        (\(nm, tr) ->
             bvar (fromString . U.fieldNameToPatName $ nm) <--
             var "Data.Swagger.declareSchemaRef" @@
             (var "Data.Proxy.Proxy" GHC.SG.@::@
              (var "Data.Proxy.Proxy" @@
               var (fromString . replaceUnitProxy $ U.referenceToQualTypeName prefix tr)))) .
        second (\(TR.Field _ tr) -> tr) <$>
        Map.toList map'
    requiredList = fmap (string . fst) $ filter (\(_, TR.Field req _) -> req) $ Map.toList map'
    propertiesList =
        (\fnm -> tuple [string fnm, var . fromString $ U.fieldNameToPatName fnm]) <$>
        Map.keys map'
    bdy = do' . reverse $ ret : binds
      where
        titleLit = maybe (var "Prelude.Nothing") (\x -> var "Prelude.Just" @@ string x) title
        ret =
            stmt $
            var "Prelude.return" @@
            (var "Data.Swagger.NamedSchema" @@ titleLit @@
             (var "Prelude.mempty" `amp`
              op
                  (var "Data.Swagger.type_")
                  "Control.Lens.?~"
                  (var "Data.Swagger.SwaggerObject") `amp`
              op (var "Data.Swagger.properties") "Control.Lens..~" (list propertiesList) `amp`
              op (var "Data.Swagger.required") "Control.Lens..~" (list requiredList)))
buildToSchemaInstance title _ typename (TR.SumType map') =
    instance'
        (var "Data.Swagger.ToSchema" @@ var (fromString typename))
        [funBind "declareNamedSchema" $ match [bvar "_proxy"] bdy]
  where
    titleLit = maybe (var "Prelude.Nothing") (\x -> var "Prelude.Just" @@ string x) title
    bdy =
        var "Prelude.return" @@
        (var "Data.Swagger.NamedSchema" @@ titleLit @@
         (var "Prelude.mempty" `amp` op (var "Data.Swagger.enum_") "Control.Lens.?~" enumList))
      where
        enumList =
            list $
            (\x -> var "Data.Aeson.String" @@ string x) . uncurry mkClause <$> Map.toList map'
        mkClause optName _ = optName
{-
instance ToSchema typename where
    declareNamedSchema _proxy = do
        return $ NamedSchema title $ mempty
            & enum_ ?~
                [Data.Yaml.String "enum1", ..., Data.Yaml.String "enumN"]
-}
buildToSchemaInstance title prefix typename tr'
    | (TR.OneOfType map') <- tr' =
        mkInstance $
        (\(_, TR.SumConstr flds) ->
             if null flds
                 then "()"
                 else case head flds -- as there is no way to have two types in one item of OneOf list in json schema
                            of
                          TR.Field _ tr -> U.referenceToQualTypeName prefix tr) <$>
        Map.toList map'
    | (TR.AnyOfType set') <- tr' =
        mkInstance $ U.referenceToQualTypeName prefix <$> Set.toList set'
  where
    mkInstance :: [U.QualTypeName] -> HsDecl'
    mkInstance ts =
        instance'
            (var "Data.Swagger.ToSchema" @@ var (fromString typename))
            [funBind "declareNamedSchema" $ match [bvar "_proxy"] bdy]
      where
        titleLit = maybe (var "Prelude.Nothing") (\x -> var "Prelude.Just" @@ string x) title
        bdy = do' . reverse $ ret : binds
        toBind = \x -> "alternative" ++ show x
        toBind' = (++ "'") . toBind
        binds =
            (\(t, i) ->
                 bvar (fromString $ toBind' i) <-- var "Data.Swagger.declareSchemaRef" @@
                 (var "Data.Proxy.Proxy" GHC.SG.@::@
                  (var "Data.Proxy.Proxy" @@ var (fromString . replaceUnitProxy $ t)))) <$>
            zip ts [1 :: Int ..]
        ret =
            stmt $
            var "Prelude.return" @@
            (var "Data.Swagger.NamedSchema" @@ titleLit @@
             (var "Prelude.mempty" `amp`
              op
                  (var "Data.Swagger.type_")
                  "Control.Lens.?~"
                  (var "Data.Swagger.SwaggerObject") `amp`
              op (var "Data.Swagger.properties") "Control.Lens..~" (list propertiesList) `amp`
              op (var "Data.Swagger.required") "Control.Lens..~" (list [])))
          where
            propertiesList =
                (\i -> tuple [string . toBind $ i, var (fromString . toBind' $ i)]) <$>
                [1 .. length ts]
{-
instance ToSchema typename where
    declareNamedSchema _proxy = do
        k1 <-  declareSchemaRef (Proxy :: Proxy ''k1)
        ....
        kn <-  declareSchemaRef (Proxy :: Proxy ''k1)
        return $ NamedSchema title $ mempty
            & type_ ?~ SwaggerObject
            & properties .~
                [("k1_oneOf_alternative", ''k1), ..., ("kn_oneOf_alternative", ''kn)]
            & required .~ []

-}
buildToSchemaInstance title prefix typename (TR.AllOfType set) =
    instance'
        (var "Data.Swagger.ToSchema" @@ var (fromString typename))
        [funBind "declareNamedSchema" $ match [bvar "_proxy"] bdy]
  where
    toBind = \i -> "part" ++ show i
    binds =
        (\(i, tr) ->
             bvar (fromString $ toBind i) <-- var "Data.Swagger.declareSchemaRef" @@
             (var "Data.Proxy.Proxy" GHC.SG.@::@
              (var "Data.Proxy.Proxy" @@
               var (fromString . replaceUnitProxy $ U.referenceToQualTypeName prefix tr)))) <$>
        zip [1 ..] (Set.toList set)
    altList = list $ var . fromString . toBind <$> [1 .. Set.size set]
    bdy = do' . reverse $ ret : binds
      where
        titleLit = maybe (var "Prelude.Nothing") (\x -> var "Prelude.Just" @@ string x) title
        ret =
            stmt $
            var "Prelude.return" @@
            (var "Data.Swagger.NamedSchema" @@ titleLit @@
             (var "Prelude.mempty" `amp`
              op (var "Data.Swagger.allOf") "Control.Lens.?~" altList))
buildToSchemaInstance title prefix typename (TR.ArrayType tr') =
    instance'
        (var "Data.Swagger.ToSchema" @@ var (fromString typename))
        [funBind "declareNamedSchema" $ match [bvar "_proxy"] bdy]
  where
    bind =
        bvar "itemSchema" <-- var "Data.Swagger.declareSchemaRef" @@
        (var "Data.Proxy.Proxy" GHC.SG.@::@
         (var "Data.Proxy.Proxy" @@
          var (fromString . replaceUnitProxy $ U.referenceToQualTypeName prefix tr')))
    bdy = do' [bind, ret]
      where
        titleLit = maybe (var "Prelude.Nothing") (\x -> var "Prelude.Just" @@ string x) title
        ret =
            stmt $
            var "Prelude.return" @@
            (var "Data.Swagger.NamedSchema" @@ titleLit @@
             (var "Prelude.mempty" `amp`
              op (var "Data.Swagger.type_") "Control.Lens.?~" (var "Data.Swagger.SwaggerArray") `amp`
              op
                  (var "Data.Swagger.items")
                  "Control.Lens.?~"
                  (var "Data.Swagger.SwaggerItemsObject" @@ var "itemSchema")))
buildToSchemaInstance title prefix typename (TR.NewType tr') =
    instance'
        (var "Data.Swagger.ToSchema" @@ var (fromString typename))
        [funBind "declareNamedSchema" $ match [bvar "_proxy"] bdy]
  where
    titleLit = maybe (var "Prelude.Nothing") (\x -> var "Prelude.Just" @@ string x) title
    proxy =
        var "Data.Proxy.Proxy" GHC.SG.@::@
        (var "Data.Proxy.Proxy" @@ var (fromString . replaceUnitProxy $ U.referenceToQualTypeName prefix tr'))
    bdy =
        var "Prelude.return" @@
        ((var "Data.Swagger.Internal.Schema.toNamedSchema" @@ proxy) `amp`
         op (var "Data.Swagger.name") "Control.Lens..~" titleLit)
buildToSchemaInstance title _ typename (TR.Ref (TR.RefPrimitiveType "Data.TransportTypes.Utils.UnitProxy")) =
    instance'
        (var "Data.Swagger.ToSchema" @@ var (fromString typename))
        [funBind "declareNamedSchema" $ match [bvar "_proxy"] bdy]
  where
    titleLit = maybe (var "Prelude.Nothing") (\x -> var "Prelude.Just" @@ string x) title
    bdy =
        var "Prelude.return" @@
        (var "Data.Swagger.NamedSchema" @@ titleLit @@
         (var "Prelude.mempty" `amp`
          op (var "Data.Swagger.type_") "Control.Lens.?~" (var "Data.Swagger.SwaggerNull")))
buildToSchemaInstance _ _ typename (TR.Ref _) =
    instance' (var "Data.Swagger.ToSchema" @@ var (fromString typename)) []
buildToSchemaInstance title _ typename (TR.ConstType va) =
    instance'
        (var "Data.Swagger.ToSchema" @@ var (fromString typename))
        [funBind "declareNamedSchema" $ match [bvar "_proxy"] bdy]
  where
    titleLit = maybe (var "Prelude.Nothing") (\x -> var "Prelude.Just" @@ string x) title
    bdy =
        var "Prelude.return" @@
        (var "Data.Swagger.NamedSchema" @@ titleLit @@
         (var "Prelude.mempty" `amp` op (var "Data.Swagger.enum_") "Control.Lens.?~" enumList))
      where
        enumList =
            list . pure $
            var "Prelude.maybe" @@
            (var "Prelude.error" @@
             string "Something is very wrong. Could not decode value stored in const") @@
            var "Prelude.id" @@
            (var "Data.Aeson.decodeStrict'" @@
             string (T.unpack . decodeUtf8 . toStrict $ encode va))
{-
instance ToSchema typename where
    declareNamedSchema _proxy = do
        return $ NamedSchema title $ mempty
            & enum_ ?~
                [va]
-}

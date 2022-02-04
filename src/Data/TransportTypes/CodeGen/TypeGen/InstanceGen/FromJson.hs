module Data.TransportTypes.CodeGen.TypeGen.InstanceGen.FromJson
    ( buildFromJSONInstance
    ) where

import GHC.SourceGen (App (op, (@@)), BVar (bvar), HsDecl', Var (var), case', conP, do',
                      funBind, funBinds, if', instance', match, matchGRHSs, recordConE, rhs,
                      stmt, string, valBind, where', wildP, (<--))

import qualified Data.Map.Strict as Map
import           Data.String     (IsString (fromString))
import qualified Data.Text       as T

import           Data.Text.Encoding                      (decodeUtf8)
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import qualified Data.TransportTypes.TypeRep             as TR
import           Data.Yaml                               (Value (String), encode)

buildFromJSONInstance :: TR.TypeName -> TR.TypeRep -> HsDecl'
buildFromJSONInstance typename (TR.ProdType map' b)
    | Map.null map' =
        let res =
                if b
                    then op (var "Prelude.return") "Prelude.." (var (fromString typename))
                    else var "Prelude.const" @@
                         (var "Prelude.return" @@ var (fromString typename))
         in instance'
                (var "Data.Yaml.FromJSON" @@ var (fromString typename))
                [ funBind "parseJSON" $
                  match [] $ var "Data.Yaml.withObject" @@ string typename @@ res
                ]
    | otherwise =
        instance'
            (var "Data.Yaml.FromJSON" @@ var (fromString typename))
            [funBind "parseJSON" $ matchGRHSs [] $ rhs decl `where'` [func]]
  where
    decl = var "Data.Yaml.withObject" @@ string typename @@ var "f"
    parseStmts fieldName (TR.Field True _) =
        bvar (fromString . U.fieldNameToPatName $ fieldName) <--
        op (var "obj") "Data.Yaml..:" (string fieldName)
    parseStmts fieldName (TR.Field False _) =
        bvar (fromString . U.fieldNameToPatName $ fieldName) <--
        op (var "obj") "Data.Yaml..:?" (string fieldName)
    func =
        funBind "f" $
        match [bvar "obj"] $
        do' $
        maybe id (:) getAdditionalProperties (uncurry parseStmts <$> Map.toList map') ++
        [ stmt $
          var "Prelude.return" @@
          recordConE
              (fromString typename)
              (maybe
                   id
                   (:)
                   recUpdAdditionalProperties
                   ((\k ->
                         ( fromString . U.changeReservedNames $ k
                         , var . fromString . U.fieldNameToPatName $ k)) <$>
                    Map.keys map'))
        ]
      where
        parseAdditionalPropertiesFieldName = "additionalProperties"
        parseAdditionalPropertiesPat = U.fieldNameToPatName parseAdditionalPropertiesFieldName
        recUpdAdditionalProperties =
            if b
                then Just
                         ( fromString parseAdditionalPropertiesFieldName
                         , var . fromString $ parseAdditionalPropertiesPat)
                else Nothing
        getAdditionalProperties =
            if b
                then Just $
                     (bvar . fromString $ parseAdditionalPropertiesPat) <--
                     var "Prelude.return" @@
                     var "obj"
                else Nothing
buildFromJSONInstance typename (TR.SumType map')
    | null map' =
        instance'
            (var "Data.Yaml.FromJSON" @@ var (fromString typename))
            [funBind "parseJSON" $ match [] $ var "Prelude.fail" @@ string "no constructors"]
    | otherwise =
        instance'
            (var "Data.Yaml.FromJSON" @@ var (fromString typename))
            [ funBind "parseJSON" $
              matchGRHSs [] $
              rhs (var "Data.Yaml.withText" @@ string typename @@ var "f") `where'` [func]
            ]
  where
    func =
        funBinds "f" . reverse $
        match
            [bvar "s"]
            (var "Prelude.fail" @@
             op
                 (string "no such constructor:")
                 "Prelude.++"
                 (var "Data.Text.unpack" @@ var "s")) :
        ((\x ->
              match [string x] $
              var "Prelude.return" @@ var (fromString . U.fieldNameToSumCon $ x)) <$>
         fmap fst map')
buildFromJSONInstance typename (TR.OneOfType map') =
    instance'
        (var "Data.Yaml.FromJSON" @@ var (fromString typename))
        [ funBind "parseJSON" $
          match [bvar "obj"] $
          foldr
              (`op` "Control.Applicative.<|>")
              (var "Prelude.fail" @@ string ("all options failed for " ++ typename))
              opts
        ]
  where
    opts =
        (\optName ->
             var "Prelude.fmap" @@ var (fromString optName) @@
             (var "Data.Yaml.parseJSON" @@ var "obj")) <$>
        fmap fst map'
buildFromJSONInstance typename (TR.AnyOfType set') =
    instance'
        (var "Data.Yaml.FromJSON" @@ var (fromString typename))
        [funBind "parseJSON" $ match [bvar "obj"] $ do' . reverse $ ret : binds]
  where
    bindNames = ("opt" ++) . show <$> [1 .. length set']
    binds =
        (\x ->
             (bvar . fromString . U.fieldNameToPatName $ x) <--
             op
                 (var "Prelude.fmap" @@ var "Prelude.Just" @@
                  (var "Data.Yaml.parseJSON" @@ var "obj"))
                 "Control.Applicative.<|>"
                 (var "Prelude.pure" @@ var "Prelude.Nothing")) <$>
        bindNames
    ret =
        stmt $
        var "Prelude.return" @@
        foldl
            (\acc new ->
                 acc @@ op (var "Data.TransportTypes.Utils.emptynessCheck") "Prelude.=<<" new)
            (var (fromString typename))
            (var . fromString . U.fieldNameToPatName <$> bindNames)
buildFromJSONInstance typename (TR.AllOfType set') =
    instance'
        (var "Data.Yaml.FromJSON" @@ var (fromString typename))
        [funBind "parseJSON" $ match [bvar "obj"] $ do' . reverse $ ret : binds]
  where
    bindNames = ("opt" ++) . show <$> [1 .. length set']
    binds =
        (\x ->
             (bvar . fromString . U.fieldNameToPatName $ x) <-- var "Data.Yaml.parseJSON" @@
             var "obj") <$>
        bindNames
    ret =
        stmt $
        var "Prelude.return" @@
        foldl
            (@@)
            (var (fromString typename))
            (var . fromString . U.fieldNameToPatName <$> bindNames)
buildFromJSONInstance typename (TR.ArrayType _) =
    instance'
        (var "Data.Yaml.FromJSON" @@ var (fromString typename))
        [ funBind "parseJSON" $
          match [bvar "vec"] $
          op (var (fromString typename)) "Prelude.<$>" (var "Data.Yaml.parseJSON" @@ var "vec")
        ]
buildFromJSONInstance typename (TR.NewType _) =
    instance'
        (var "Data.Yaml.FromJSON" @@ var (fromString typename))
        [ funBind "parseJSON" $
          match [bvar "v"] $
          op (var (fromString typename)) "Prelude.<$>" (var "Data.Yaml.parseJSON" @@ var "v")
        ]
buildFromJSONInstance typename (TR.Ref _) =
    instance'
        (var "Data.Yaml.FromJSON" @@ var (fromString typename))
        [ funBind "parseJSON" $
          match [bvar "v"] $
          op (var (fromString typename)) "Prelude.<$>" (var "Data.Yaml.parseJSON" @@ var "v")
        ]
buildFromJSONInstance typename (TR.ConstType v) =
    instance'
        (var "Data.Yaml.FromJSON" @@ var (fromString typename))
        [ funBind "parseJSON" $
          matchGRHSs [bvar "v"] $ rhs decodeValidation `where'` [bindGroundTruth]
        ]
  where
    decodeValidation = do' [bvar "cnd" <-- cond, stmt $ if' (var "cnd") good bad]
      where
        cond =
            case'
                (var "groundTruth")
                [ match [conP "Prelude.Left" [wildP]] constFail
                , match [conP "Prelude.Right" [bvar "x"]] cmp
                ]
        cmp = var "Prelude.return" @@ op (var "x") "Prelude.==" (var "v")
        good = var "Prelude.return" @@ var (fromString cntrName)
        constFail =
            var "Prelude.fail" @@
            string ("Something is very wrong. Invalid value inside const " ++ typename)
        bad =
            var "Prelude.fail" @@
            op
                (string
                     ("Failed on parsing const: " ++
                      typename ++ ". " ++ "groundTruth: " ++ show v ++ " "))
                "Prelude.++"
                (op (string "sample: ") "Prelude.++" (var "Prelude.show" @@ var "v"))
    bindGroundTruth =
        valBind "groundTruth" $
        var "Data.Yaml.decodeThrow" @@ string (T.unpack . decodeUtf8 . encode $ v)
    cntrName =
        case v of
            String txt -> U.fieldNameToSumCon $ T.unpack txt
            _other     -> typename

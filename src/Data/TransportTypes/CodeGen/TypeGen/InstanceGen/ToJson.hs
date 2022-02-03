module Data.TransportTypes.CodeGen.TypeGen.InstanceGen.ToJson
    ( buildToJSONInstance
    ) where

import qualified Data.Map.Strict                         as Map
import qualified Data.Set                                as Set
import           Data.String                             (IsString (fromString))
import qualified Data.Text                               as T
import           Data.Text.Encoding                      (decodeUtf8)
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import qualified Data.TransportTypes.TypeRep             as TR
import           Data.Yaml                               (encode)
import           GHC.SourceGen                           (App (op, (@@)), BVar (bvar),
                                                          HasList (list, nil), HsDecl',
                                                          HsExpr', RawMatch, Var (var), case',
                                                          conP, funBind, funBinds, instance',
                                                          lambda, let', match, matchGRHSs, rhs,
                                                          string, tuple, tyApp, valBind,
                                                          where', wildP)

buildToJSONInstance :: TR.TypeName -> TR.TypeRep -> HsDecl'
buildToJSONInstance typename (TR.ProdType map' b) =
    instance'
        (var "Data.Yaml.ToJSON" @@ var (fromString typename))
        [funBind "toJSON" $ match [patternMatch] objectCntr]
  where
    additionalPropertiesFieldName = "additionalProperties"
    additionalPropFilter x = not $ b && x == additionalPropertiesFieldName
    patternMatch =
        conP (fromString typename) $
        let binds = bvar . fromString . U.fieldNameToPatName <$> Map.keys map'
         in if b
                then (bvar . fromString . U.fieldNameToPatName $ additionalPropertiesFieldName) :
                     binds
                else binds
    objectCntr =
        var "Data.Yaml.object" @@
        addAdditionalPropertiesToObj (var "Data.Maybe.catMaybes" @@ list pairs)
      where
        tupling :: String -> HsExpr'
        tupling key = lambda [bvar "x"] (tuple [string key, var "Data.Yaml.toJSON" @@ var "x"])
        myabeWrap req nm =
            let k = (fromString $ U.fieldNameToPatName nm)
             in if req
                    then var "Prelude.Just" @@ var k
                    else var "Prelude.id" @@ var k
        pairs =
            (\(k, TR.Field req _) -> var "Prelude.fmap" @@ tupling k @@ myabeWrap req k) <$>
            filter (additionalPropFilter . fst) (Map.toList map')
    addAdditionalPropertiesToObj objList =
        if b
            then let additionalPairs =
                         var "Data.HashMap.Strict.toList" @@
                         var
                             (fromString $
                              U.fieldNameToPatName additionalPropertiesFieldName)
                  in op objList "Prelude.++" additionalPairs
            else objList
buildToJSONInstance typename (TR.SumType map') =
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
buildToJSONInstance typename (TR.OneOfType map') =
    instance'
        (var "Data.Yaml.ToJSON" @@ var (fromString typename))
        [funBinds "toJSON" $ uncurry mkClause <$> Map.toList map']
    -- flds are treated if they have at most one entity as it is impossible to do otherwise in json
    -- in future in migth be wise to make other case unrepresentable.
  where
    mkClause :: TR.FieldName -> TR.SumConstr -> RawMatch
    mkClause optName (TR.SumConstr flds)
        | [] <- flds = match [wildP] $ var "Data.Yaml.Null"
        | otherwise =
            match [conP (fromString . U.fieldNameToSumCon $ optName) [bvar "x"]] $
            var "Data.Yaml.toJSON" @@ var "x"
buildToJSONInstance typename (TR.AnyOfType set')
    | Set.null set' =
        instance'
            (var "Data.Yaml.ToJSON" @@ var (fromString typename))
            [funBind "toJSON" $ match [bvar "x"] (var . fromString $ typename)] -- maybe null is better?
    | Set.size set' == 1 =
        instance'
            (var "Data.Yaml.ToJSON" @@ var (fromString typename))
            [ funBind "toJSON" $
              match
                  [conP (fromString typename) [bvar "x"]]
                  (case'
                       (var "Prelude.fmap" @@ var "Data.Yaml.toJSON" @@ var "x")
                       [ match [conP "Prelude.Nothing" []] $ var "Data.Yaml.Null"
                       , match [conP "Prelude.Just" [bvar "x'"]] $ var "x'"
                       ])
            ]
    | otherwise = instance' (var "Data.Yaml.ToJSON" @@ var (fromString typename)) [decl]
  where
    bindNames = (\n -> "part" ++ show n) <$> [1 .. Set.size set']
    patternMatch = conP (fromString typename) $ bvar . fromString <$> bindNames
    decl =
        funBind "toJSON" $
        matchGRHSs [patternMatch] $
        rhs
            (let' [valBind "parts" existingParts] $
             case'
                 (var "parts")
                 [ match [nil] $ var "Data.Yaml.Null"
                 , match
                       [wildP]
                       (var "Data.Foldable.foldl'" @@ var "merge" @@
                        (var "Prelude.head" @@ var "parts") @@
                        (var "Prelude.tail" @@ var "parts"))
                 ]) `where'`
        [mergeFunc]
      where
        existingParts = var "Data.Maybe.catMaybes" @@ list parts
        parts =
            (\bnm -> var "Prelude.fmap" @@ var "Data.Yaml.toJSON" @@ (var . fromString $ bnm)) <$>
            bindNames
        mergeFunc =
            funBinds
                "merge"
                [ match
                      [ conP "Data.Yaml.Object" [bvar "lo"]
                      , conP "Data.Yaml.Object" [bvar "ro"]
                      ] $
                  var "Data.Yaml.Object" @@ op (var "ro") "Prelude.<>" (var "lo")
                , match
                      [bvar "lv", bvar "rv"]
                      (case'
                           (op (var "lv") "Prelude.==" (var "rv"))
                           [ match [bvar "Prelude.True"] (var "lv")
                           , match [bvar "Prelude.False"] (var "Data.Yaml.Null")
                           ])
                ]
buildToJSONInstance typename (TR.AllOfType set)
    | Set.null set =
        instance'
            (var "Data.Yaml.ToJSON" @@ var (fromString typename))
            [funBind "toJSON" $ match [wildP] $ string typename]
    | Set.size set == 1 =
        instance'
            (var "Data.Yaml.ToJSON" @@ var (fromString typename))
            [ funBind "toJSON" $
              match [conP (fromString typename) [bvar "x"]] $ var "toJSON" @@ var "x"
            ]
    | otherwise = instance' (var "Data.Yaml.ToJSON" @@ var (fromString typename)) [decl]
  where
    bindNames = (\x -> "opt" ++ show x) <$> [1 .. (Set.size set)]
    decl =
        funBind "toJSON" $
        matchGRHSs [conP (fromString typename) (bvar . fromString <$> bindNames)] $
        rhs
            (let'
                 [ valBind "objs" $
                   list
                       ((\n -> var "Data.Yaml.toJSON" @@ (var . fromString $ n)) <$> bindNames)
                 ]
                 (var "Data.Foldable.foldl'" @@ var "merge" @@
                  (var "Prelude.head" @@ var "objs") @@
                  (var "Prelude.tail" @@ var "objs"))) `where'`
        [ funBinds
              "merge"
              [ match
                    [conP "Data.Yaml.Object" [bvar "lo"], conP "Data.Yaml.Object" [bvar "ro"]] $
                var "Data.Yaml.Object" @@ op (var "ro") "Prelude.<>" (var "lo")
              , match
                    [bvar "lv", bvar "rv"]
                    (case'
                         (op (var "lv") "Prelude.==" (var "rv"))
                         [ match [bvar "Prelude.True"] (var "lv")
                         , match [bvar "Prelude.False"] (var "Data.Yaml.Null")
                         ])
              ]
        ]
buildToJSONInstance typename (TR.ArrayType _) =
    instance' (var "Data.Yaml.ToJSON" @@ var (fromString typename)) [decl]
  where
    decl =
        funBind "toJSON" $
        match [conP (fromString typename) [bvar "vec"]] $
        var "Data.Yaml.Array" @@ (var "Prelude.fmap" @@ var "Data.Yaml.toJSON" @@ var "vec")
buildToJSONInstance typename (TR.NewType _) =
    instance' (var "Data.Yaml.ToJSON" @@ var (fromString typename)) [decl]
  where
    decl =
        funBind "toJSON" $
        match [conP (fromString typename) [bvar "x"]] $ var "Data.Yaml.toJSON" @@ var "x"
buildToJSONInstance typename (TR.Ref _) =
    instance' (var "Data.Yaml.ToJSON" @@ var (fromString typename)) [decl]
  where
    decl =
        funBind "toJSON" $
        match [conP (fromString typename) [bvar "x"]] $ var "Data.Yaml.toJSON" @@ var "x"
buildToJSONInstance typename (TR.ConstType va) =
    instance'
        (var "Data.Yaml.ToJSON" @@ var (fromString typename))
        [funBind "toJSON" $ match [wildP] decl]
  where
    decl =
        case'
            (var "Data.Yaml.decodeEither'" `tyApp` var "Data.Yaml.Value" @@
             string (T.unpack . decodeUtf8 . encode $ va))
            [ match [conP "Prelude.Left" [bvar "err"]] $
              var "Prelude.error" @@
              op
                  (string "can't decode const value. Something is very wrong: ")
                  "Prelude.++"
                  (var "Prelude.show" @@ var "err")
            , match [conP "Prelude.Right" [bvar "x"]] $ var "x"
            ]

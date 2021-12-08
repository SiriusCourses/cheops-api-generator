{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.ConfigGen.JSTypes
    ( TypeTag(.., Primitive, RecordLike, ObjectTag, EnumTag, NumberTag,
        StringTag, NullTag, BoolTag)
    , parseTypeTag
    , RecordLikeTag(..)
    , PrimitiveTag(..)
    ) where

data TypeTag
    = Rec RecordLikeTag
    | Prim PrimitiveTag
    | ArrayTag

{-# COMPLETE ArrayTag, StringTag, NumberTag, BoolTag, NullTag,
  ObjectTag, EnumTag #-}

data PrimitiveTag
    = PrimStringTag
    | PrimNumberTag
    | PrimBoolTag
    | PrimNullTag

data RecordLikeTag
    = RecEnumTag
    | RecObjectTag

pattern ObjectTag :: TypeTag

pattern ObjectTag = Rec RecObjectTag

pattern EnumTag :: TypeTag

pattern EnumTag = Rec RecEnumTag

pattern StringTag :: TypeTag

pattern StringTag = Prim PrimStringTag

pattern NumberTag :: TypeTag

pattern NumberTag = Prim PrimNumberTag

pattern BoolTag :: TypeTag

pattern BoolTag = Prim PrimBoolTag

pattern NullTag :: TypeTag

pattern NullTag = Prim PrimNullTag

isPrimitive :: TypeTag -> Bool
isPrimitive ObjectTag = False
isPrimitive EnumTag   = False
isPrimitive ArrayTag  = False
isPrimitive _         = True

pattern Primitive :: TypeTag

pattern Primitive <- (isPrimitive -> True)

pattern RecordLike :: TypeTag

pattern RecordLike <- (isPrimitive -> False)

{-# COMPLETE Primitive, RecordLike #-}

parseTypeTag :: String -> Maybe TypeTag
parseTypeTag "array"  = Just ArrayTag
parseTypeTag "object" = Just ObjectTag
parseTypeTag "enum"   = Just EnumTag
parseTypeTag "number" = Just NumberTag
parseTypeTag "string" = Just StringTag
parseTypeTag "text"   = Just StringTag
parseTypeTag "bool"   = Just BoolTag
parseTypeTag "null"   = Just NullTag
parseTypeTag _        = Nothing

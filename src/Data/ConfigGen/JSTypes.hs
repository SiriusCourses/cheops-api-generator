{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.ConfigGen.JSTypes
    ( TypeTag(.., Primitive, RecordLike, ObjectTag, EnumTag, IntTag,
        DoubleTag, StringTag, NullTag, BoolTag)
    , parseTypeTag
    , RecordLikeTag(..)
    , PrimitiveTag(..)
    ) where

data TypeTag
    = Rec RecordLikeTag
    | Prim PrimitiveTag
    | ArrayTag

{-# COMPLETE ArrayTag, StringTag, IntTag, DoubleTag, BoolTag,
  NullTag, ObjectTag, EnumTag #-}

data PrimitiveTag
    = PrimStringTag
    | PrimIntTag
    | PrimDoubleTag
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

pattern IntTag :: TypeTag

pattern IntTag = Prim PrimIntTag

pattern DoubleTag :: TypeTag

pattern DoubleTag = Prim PrimDoubleTag

pattern BoolTag :: TypeTag

pattern BoolTag = Prim PrimBoolTag

pattern NullTag :: TypeTag

pattern NullTag = Prim PrimNullTag

isPrimitive :: TypeTag -> Bool
isPrimitive ObjectTag = False
isPrimitive ArrayTag  = False
isPrimitive _         = True

pattern Primitive :: TypeTag

pattern Primitive <- (isPrimitive -> True)

pattern RecordLike :: TypeTag

pattern RecordLike <- (isPrimitive -> False)

{-# COMPLETE Primitive, RecordLike #-}

parseTypeTag :: String -> Maybe TypeTag
parseTypeTag "array"   = Just ArrayTag
parseTypeTag "object"  = Just ObjectTag
parseTypeTag "number"  = Just DoubleTag
parseTypeTag "integer" = Just IntTag
parseTypeTag "string"  = Just StringTag
parseTypeTag "boolean" = Just BoolTag
parseTypeTag "null"    = Just NullTag
parseTypeTag _         = Nothing

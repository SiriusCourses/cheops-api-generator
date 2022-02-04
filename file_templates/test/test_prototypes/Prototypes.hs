{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Prototypes where

import Control.Monad       (unless)

import qualified Codec.Binary.UTF8.String
import           Data.ByteString          (ByteString, unpack)
import           Data.ByteString.Lazy     (toStrict)

import Data.Aeson (Value, encode)
import Data.Yaml  (FromJSON, ParseException, ToJSON, decodeEither', encode)

import qualified FFI
import           Test.QuickCheck         (Property)
import           Test.QuickCheck.Monadic (assert, monadicIO, pre, run)

import Debug.Trace (trace)

encodingDecodingInvariantTest_prototype ::
       forall a. (ToJSON a, FromJSON a, Eq a, Show a)
    => a
    -> Bool
encodingDecodingInvariantTest_prototype sample
    | Right True <- (== sample) <$> recodedSample = True
    | otherwise =
        trace
            (">>> Failed test!!!\n" ++
             "sample:\n" ++
             show sample ++
             "\nencoding:\n" ++
             encodingRepPritty ++ "\nRecoded sample:\n" ++ show recodedSample ++ "\n")
            () `seq`
        False
  where
    encodingRepPritty = Codec.Binary.UTF8.String.decode . Data.ByteString.unpack $ encodingRep
    encodingRep = Data.Yaml.encode sample
    recodedSample = Data.Yaml.decodeEither' @a encodingRep

validationTest_prototype ::
       forall a. (ToJSON a, Show a)
    => String
    -> (a -> Bool)
    -> ByteString
    -> a
    -> Property
validationTest_prototype qualTypename precond rawSchema sample =
    monadicIO $ do
        pre $ precond sample
        jsonSchema <-
            either (run . decodingFailReport "schema") return $ fromYamlToJson rawSchema
        jsonSample <-
            either (run . decodingFailReport "sample") return $
            fromYamlToJson (Data.Yaml.encode sample)
        res <- run $ FFI.validateJSON jsonSample jsonSchema
        unless res . run $ validateionFailReport jsonSample jsonSchema
        assert res
  where
    fromYamlToJson :: ByteString -> Either ParseException ByteString
    fromYamlToJson =
        fmap (toStrict . Data.Aeson.encode) . Data.Yaml.decodeEither' @Data.Aeson.Value
    decodingFailReport :: String -> ParseException -> IO ByteString
    decodingFailReport msg er = do
        putStrLn ""
        putStrLn ""
        putStrLn $ "Failed decoding of encoded " ++ msg ++ " during test for " ++ qualTypename
        putStrLn "encoded sample:"
        print sample
        putStrLn "current schema:"
        putStrLn . Codec.Binary.UTF8.String.decode . Data.ByteString.unpack $ rawSchema
        putStrLn "Error:"
        fail $ show er
    validateionFailReport :: ByteString -> ByteString -> IO ()
    validateionFailReport jsonObj jsonSch = do
        putStrLn ""
        putStrLn ""
        putStrLn $ "Failed validation during test for " ++ qualTypename
        putStrLn "json sample:"
        print jsonObj
        putStrLn "json schema:"
        putStrLn . Codec.Binary.UTF8.String.decode . Data.ByteString.unpack $ jsonSch
{-
sample:
PublicInfoResponse
    Nothing
    Just (PublicInfoResponseSuccess {success = Success []})
recSample:
PublicInfoResponse
    Just (BaseResponse {dialogue = Nothing, errors = Nothing, warnings = Nothing})
    Just (PublicInfoResponseSuccess {success = Success []})

-}

module Prototypes where

import Control.Applicative (liftA2)
import Control.Monad       (unless)

import qualified Codec.Binary.UTF8.String
import           Data.ByteString          (ByteString, unpack)
import           Data.ByteString.Lazy     (toStrict)

import Data.Aeson (Value, encode)
import Data.Yaml  (FromJSON, ParseException, ToJSON, decodeEither', encode)

import Test.QuickCheck         (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, pre, run)
import qualified FFI

prop_fromJSONInv_prot ::
       forall a. (ToJSON a, FromJSON a, Eq a)
    => a
    -> Bool
prop_fromJSONInv_prot sample
    | Right True <- single `eq` double = True
    | otherwise = False
  where
    single = Data.Yaml.decodeEither' @a . Data.Yaml.encode $ sample
    double = Data.Yaml.decodeEither' @a . Data.Yaml.encode =<< single
    eq = liftA2 (==)

prop_toJSONInv_prot ::
       forall a. (ToJSON a, Show a)
    => String
    -> (a -> Bool)
    -> ByteString
    -> a
    -> Property
prop_toJSONInv_prot qualTypename precond rawSchema sample =
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

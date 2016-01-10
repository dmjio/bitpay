{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- |
-- Module      : Network.BitPay.Util
-- Copyright   : (c) David Johnson 2015
-- Maintainer  : djohnson.m@ngmail.com
-- Stability   : experimental
-- Portability : POSIX
--
module Network.BitPay.Util where

import           Control.Arrow
import           Data.Maybe
import           Data.Word
import           Network.Haskoin.Crypto
import           Network.Haskoin.Util
import           Crypto.Secp256k1 (secKey)
import           System.Entropy (getEntropy)
import           Data.Monoid
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Aeson
import qualified Data.ByteString.Lazy as L


-- Utils
getKeyPair :: IO (PubKey, PrvKey)
getKeyPair = (derivePubKey &&& id) <$> makePrvKey . fromJust . secKey <$> getEntropy 32

-- | Make SIN
-- base58check( 0x0F + 0x02 + ripemd160( sha256( k1 ) )
makeSIN :: PubKey -> T.Text
makeSIN p = T.decodeUtf8 $ encodeBase58Check ( B.pack [0x0F, 0x02] <>
  (getHash160 $ hash160 $ getHash256 $ hash256 $ encode' p) )

signReq :: ToJSON a => a -> T.Text -> PrvKey -> T.Text
signReq x url prvKey = x
  where
    x = T.decodeUtf8 . encodeHex . encode' $ signMsg (hash256 bod) prvKey
    bod = T.encodeUtf8 url <> (L.toStrict $ encode x)

-- hash256 :: ByteString -> Hash256
-- hash256 = Hash256 . (toBytes :: Digest SHA256 -> ByteString) . hash

-- signMsg :: Hash256 -> PrvKey -> Signature
-- signMsg h d = Signature $ EC.signMsg (prvKeySecKey d) (hashToMsg h)

-- Right (Response {rData = [TokenResponse {trPolicies = [Policy {pPolicy = Id, pMethod = Inactive, pParams = Just ["Tf6kKqxfMUwVGDntpgh7japjNT3R6iD8jYd"]}], trResource = Nothing, trToken = TokenId {unTokenId = "7MSijbUZZKZd3AmhuftQUqkTQNL44U6MFePtJdu4MAsW"}, trFacade = Nothing, trDateCreated = TokenCreated {unTokenCreated = 1449013977989}, trPairingExpiration = PairingExpiration {unPairingExpiration = 1449100377989}, trPairingCode = PairingCode {unPairingCode = "tx65qQ8"}}]})
-- (0.15 secs, 58,701,152 bytes)

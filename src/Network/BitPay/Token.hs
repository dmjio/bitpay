{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
module Network.BitPay.Token where

import           Data.String
import           Data.Aeson
import           Servant.API
import           Servant.Client
import           Data.Proxy
import           Control.Monad.Trans.Either
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Control.Monad.IO.Class
import           Network.BitPay.Types
import           Network.BitPay.Util
import           Data.Maybe
import           Control.Arrow
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text (Text)

createToken :: MonadIO m => Token -> m (Either ServantError Value)
createToken token = liftIO $ runEitherT (f token)
  where
    f = client (Proxy :: Proxy TokenAPI) url
    url = bitPayURL

type TokenAPI = "tokens" :> ReqBody '[JSON] Token :> Post '[JSON] Value

go :: IO (Either ServantError Value)
go = do
  (pub, priv) <- getKeyPair
  print (pub, priv)
  createToken $ defToken (TokenId $ makeSIN pub)

-- (PubKey "02f480e18a33ac59adb5bd9a26b81f5da35769c976ff7f3f257bc65b3c8e24e31d",PrvKey "KyoMi8XGVYKev7oeuqS1bUr1VrvoaFYZSVvJ7aSYhxi4Kbowqqpa")
-- Right (Object (fromList [("data",Array (fromList [Object (fromList [("pairingCode",String "ce1WZ7h"),("token",String "F65Yct5erZfb8FF3EaAgmVatyUWU7mDYiA2R9r2wjN8A"),("dateCreated",Number 1.450634370594e12),("policies",Array (fromList [Object (fromList [("params",Array (fromList [String "TfF6ipdj7YaBZLz3zAj95DTJNohrFsm8oFY"])),("method",String "inactive"),("policy",String "id")])])),("pairingExpiration",Number 1.450720770594e12)])]))]))

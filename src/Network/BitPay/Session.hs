{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
module Network.BitPay.Session where

import Data.String
import Data.Aeson
import Servant.API
import Servant.Client
import Data.Proxy
import Control.Monad.Trans.Either
import Data.Text (Text)
import Network.BitPay.Types
import Control.Monad.IO.Class

createSession :: MonadIO m => m (Either ServantError SessionId)
createSession =
  liftIO $ runEitherT $ client (Proxy :: Proxy TokenAPI) bitPayURL

type TokenAPI = "sessions" :> Post '[JSON] SessionId

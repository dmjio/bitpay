{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
module Network.BitPay.Currency where

import Data.String
import Data.Aeson
import Servant.API
import Servant.Client
import Data.Proxy
import Control.Monad.Trans.Either
import Data.Text    (Text)
import Control.Monad.IO.Class
import Network.BitPay.Types

-- | Retrieves the list of exchange rates.
getCurrencies :: MonadIO m => m (Either ServantError Currencies)
getCurrencies = liftIO . runEitherT $ client currencyAPI bitPayURL

-- | Rate API
currencyAPI :: Proxy CurrencyAPI
currencyAPI = Proxy

type CurrencyAPI = "currencies" :> Get '[JSON] Currencies

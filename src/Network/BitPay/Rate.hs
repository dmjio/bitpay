{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
module Network.BitPay.Rate
  ( getRates
  , getRatesByCurrency
  , rateAPI
  ) where
import Data.String
import Data.Aeson
import Servant.API
import Servant.Client
import Data.Proxy
import Control.Monad.Trans.Either
import Data.Text (Text)
import Control.Monad.IO.Class
import Network.BitPay.Types

getRates' :: EitherT ServantError IO Rates
getRatesByCurrency' :: CurrencyId -> EitherT ServantError IO SingleRate
getRates' :<|> getRatesByCurrency' = client rateAPI bitPayURL

-- | Retrieves the list of exchange rates.
getRates :: MonadIO m => m (Either ServantError Rates)
getRates = liftIO $ runEitherT getRates'

-- | Retrieves the exchange rate for the given currency.
getRatesByCurrency :: MonadIO m => CurrencyId -> m (Either ServantError SingleRate)
getRatesByCurrency cid = liftIO $ runEitherT (getRatesByCurrency' cid)

-- | Rate API
rateAPI :: Proxy RateAPI
rateAPI = Proxy

type RateAPI =
   "rates" :> (
     Get '[JSON] Rates
       :<|> Capture "currency" CurrencyId :> Get '[JSON] SingleRate
     )



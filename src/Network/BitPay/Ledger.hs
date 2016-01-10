{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Network.BitPay.Ledger where

import Servant.API
import Servant.Client
import Network.BitPay.Types
import Data.Aeson
import Control.Monad.Trans.Either
import Data.Proxy
-- import Date.Time

type LedgerAPI =
    "ledgers"
  :> Capture "currency" CurrencyId
  :> QueryParam "startDate" StartDate
  :> QueryParam "endDate" EndDate
  :> Header "x-identity" Identity
  :> Header "x-signature" Signature
  :> Header "x-accept-version" Version
  :> Get '[JSON] Value

f :: CurrencyId
  -> Maybe StartDate
  -> Maybe EndDate
  -> Maybe Identity
  -> Maybe Signature
  -> Maybe Version
  -> EitherT ServantError IO Value
f = client (Proxy :: Proxy LedgerAPI) bitPayURL


go = runEitherT $ f (Just cid) (Just sd) (Just ed) (Just id') (Just sig) (Just ver)
  where
    cid = "USD"
    startDate = StartDate 

-- getLedger
--   :: MonadIO m
--   => Token
--   -> m (Either ServantError Response)
-- getLedger token = liftIO $ runEitherT (f token)
--   where
--     url = bitPayURL

-- go :: IO (Either ServantError Response)
-- go = do
--   (pub, _) <- getKeyPair
--   createToken $ defToken (TokenId $ makeSIN pub)




module Network.BitPay.Core where

import Servant.Client
import Control.Monad.Trans.Either

type API = "api" :> 

api :: Proxy API
api = Proxy

x = client api $ BaseUrl Https "www.bitpay.com" 443

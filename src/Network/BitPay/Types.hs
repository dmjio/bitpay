{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- |
-- Module      : Network.BitPay.Types
-- Copyright   : (c) David Johnson 2015
-- Maintainer  : djohnson.m@ngmail.com
-- Stability   : experimental
-- Portability : POSIX
--
module Network.BitPay.Types where

import           Control.Monad
import           Data.Aeson
import           Data.Char
import           Data.Aeson.Types
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Time
import           GHC.Generics
import           Servant.Common.Text
import           Servant.Client
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Monoid


-- | BitPay URL
bitPayURL :: BaseUrl
bitPayURL = BaseUrl Https "bitpay.com" 443

-- | Resource id
newtype BillId = BillId { unBillId :: Text } deriving (Show, Eq, ToJSON, FromJSON)

newtype Created = Created { unCreated :: UTCTime } deriving (Show, Eq, ToJSON, FromJSON)
newtype TokenCreated = TokenCreated { unTokenCreated :: Integer }
  deriving (Show, Eq, ToJSON, FromJSON)
newtype Delivered = Delivered { unDelivered :: Bool } deriving (Show, Eq, ToJSON, FromJSON)
newtype BillNumber = BillNumber { unBillNumber :: Text } deriving (Show, Eq, ToJSON, FromJSON)

newtype ShowRate = ShowRate { unShowRate :: Bool } deriving (Show, Eq, ToJSON, FromJSON)

newtype SessionId = SessionId { sessionData :: Text } deriving (Show, Generic)

instance FromJSON SessionId where
  parseJSON = genericParseJSON defaultOptions {
       fieldLabelModifier = drop 7 . map toLower
    }

newtype Name = Name { unName :: Text } deriving (Show, Eq, ToJSON, FromJSON)
newtype AddressOne = AddressOne { unAddressOne :: Text } deriving (Show, Eq)
newtype AddressTwo = AddressTwo { unAddressTwo :: Text } deriving (Show, Eq)
newtype DueDate = DueDate { unDueDate :: UTCTime } deriving (Show, Eq)
data BillStatus = Draft | Sent | Paid | Complete deriving Show
newtype Description = Description { unDescription :: Text } deriving (Show, Eq)
newtype Price = Price { unPrice :: Integer } deriving (Show, Eq)
newtype Archived = Archived { unArchived :: Text } deriving (Show, Eq)
newtype City = City { unCity :: Text } deriving (Show, Eq)
newtype State = State { unState :: Text } deriving (Show, Eq)
newtype Zip = Zip { unZip :: Text } deriving (Show, Eq)
newtype Country = Country { unCountry :: Text } deriving (Show, Eq)
newtype Email = Email { unEmail :: Text } deriving (Show, Eq)
newtype Phone = Phone { unPhone :: Text } deriving (Show, Eq)
newtype Quantity = Quantity { unQuantity :: Integer } deriving (Show, Eq)
newtype CurrencyAlts = CurrencyAlts { unCurrencyAlts :: Text } deriving (Show, Eq, FromJSON)
newtype OrderId = OrderId { unOrderId :: Text } deriving (Show, Eq)
newtype FullNotifications = FullNotifications { unFullNotifications :: Text } deriving (Show, Eq)
newtype URL = URL { unURL :: Text } deriving (Show, Eq)

data TransactionSpeed = High | Medium | Low deriving (Show, Eq)

-- | Bill
data Bill = Bill {
     billId :: BillId -- ^ Resource id
   , billToken :: Token -- ^ API token for bill resource
   , billCreatedDate :: Created -- ^ Time of bill creation
   , billDelivered :: Delivered -- ^ Indicates whether bill has been delivered to buyer
   , billNumber :: BillNumber -- ^ Bill identifier, specified by merchant
   , billStatus :: BillStatus -- ^ Can be `draft`, `sent`, `paid`, or `complete`
   , billCurrency :: CurrencyId -- ^ ISO 4217 3-character currency code
   , billShowRate :: ShowRate -- ^ Indicates whether corresponding invoice web page should display equivalent fiat amount
   , billArchived :: Archived -- ^ Indicates whether bill is visible in BitPay website
   , billName :: Name -- ^ Buyer Name
   , billAddress1 :: AddressOne -- ^ Buyer Street Address
   , billAddress2 :: AddressTwo -- ^ Buyer Apartment or Suite Number
   , billCity :: City -- ^ Buyer Locality or City
   , billState :: State -- ^ Buyer State or province
   , billZip :: Zip -- ^ Buyer Zip or Postal Code
   , billCountry :: Country -- ^ Buyer Country Code (ISO 3166-1 alpha-2)
   , billEmail :: Email -- ^ Buyer Email
   , billPhone :: Phone -- ^ Buyer Phone
   , billDueDate :: DueDate -- ^ UTC date, ISO-8601 format yyyy-mm-dd or yyyy-mm-ddThh:mm:ssZ. Default is current time
   , billItems :: [BillItem]
  } deriving (Show, Generic)

-- | Bill Line Item
data BillItem = BillItem {
    billItemDescription :: Description -- ^ Line item description
  , billItemPrice :: Price -- ^ Line item unit price, in `currency`
  , billQuantity :: Quantity -- ^ Line item number of units
  } deriving (Show, Eq, Generic)


newtype ClientId = ClientId { unClientId :: Text } deriving (Show, Eq, ToJSON, FromJSON)
newtype Label = Label { unLabel :: Text } deriving (Show, Eq, ToJSON, FromJSON)
newtype Approved = Approved { unApproved :: Bool } deriving (Show, Eq, ToJSON, FromJSON)
newtype Disabled = Disabled { unDisabled :: Bool } deriving (Show, Eq, ToJSON, FromJSON)

-- | Client
data Client = Client {
    clientId :: ClientId
  , clientLabel :: Label
  , clientApproved :: Approved
  , clientDisabled :: Disabled
  , clientToken :: Token
  } deriving (Show, Generic)

newtype Code = Code { unCode :: Text } deriving (Show, Eq, ToJSON, FromJSON)
newtype CurrencySymbol = CurrencySymbol { unCurrencySymbol :: Text } deriving (Show, Eq, FromJSON, ToJSON)
newtype Precision = Precision { unPrecision :: Double } deriving (Show, Eq, ToJSON, FromJSON)
newtype ExchangePctFee = ExchangePctFee { unExchangePctFee :: Double } deriving (Show, Eq, ToJSON, FromJSON)
newtype PayoutEnabled = PayoutEnabled { unPayoutEnabledp :: Bool } deriving (Show, Eq, ToJSON, FromJSON)
newtype CurrencyName = CurrencyName { unCurrencyName :: Text } deriving (Show, Eq, ToJSON, FromJSON)
newtype CurrencyPlural = CurrencyPlural { unCurrencyPlural :: Text } deriving (Show, Eq, ToJSON, FromJSON)
newtype CurrencyMinimum = CurrencyMinimum { unCurrencyMinimum :: Double } deriving (Show, Eq , ToJSON, FromJSON)
newtype PayoutField = PayoutField { unPayoutField :: Text } deriving (Show, Eq, ToJSON, FromJSON)

-- | Currencies
data Currency = Currency {
     currencyCode :: Code -- ^ ISO 4217 3-character currency code
   , currencySymbol :: CurrencySymbol -- ^ Display symbol
   , currencyPrecision :: Precision -- ^ Number of decimal places
   , currencyExchangePctFee :: ExchangePctFee -- ^ Basis points
   , currencyPayoutEnabled :: PayoutEnabled -- ^ Indicates whether BitPay can exchange currency for BTC
   , currencyName :: CurrencyName -- ^ English currency name
   , currencyPlural :: CurrencyPlural -- ^ Plural English currency name
   , currencyAlts :: CurrencyAlts -- ^ Alternative currency name(s)
   , currencyMinimum :: CurrencyMinimum -- ^ Minimum supported value
   , currencySanctioned :: CurrencySanctioned -- ^ Minimum supported value
   , currencyPayoutFields :: [PayoutField] -- ^ Can be `merchantEIN`
   , currencySettlementMinimum :: Maybe SettlementMinimum -- ^ Can be `merchantEIN`
  } deriving (Show, Eq, Generic)

-- newtype ReqUrl = ReqUrl T.Text deriving (Show, Eq, Ord) 

-- instance HasClient cli => HasClient (ReqUrl :> cli) where
--   type Client (ReqUrl :> cli) = ReqUrl -> Client cli
--   clientWithRoute p req BaseUrl{..} = clientWithRoute (Proxy :: Proxy cli) 

newtype Signature = Signature { unSignature :: Text } deriving (Show, Eq, ToText)

newtype Version = Version { unVersion :: Text } deriving (Show, Eq, FromText, ToText)

version :: Version
version = Version "2.0.0"

newtype Identity = Identity { unIdentity :: Text } deriving (Show, Eq, ToText, FromText)

newtype SettlementMinimum = SettlementMinimum { unSettlementMinimum :: Double } deriving (Show, Eq, FromJSON)

newtype CurrencySanctioned = CurrencySanctioned {
    unCurrencySanctioned :: Bool
  } deriving (Show, Eq, FromJSON)

newtype Currencies = Currencies { currenciesData :: [Currency] }
  deriving (Show, Eq, Generic)

instance FromJSON Currencies where
  parseJSON = genericParseJSON defaultOptions {
       fieldLabelModifier = map toLower . drop 10
    }

instance FromJSON Currency where
  parseJSON = genericParseJSON defaultOptions {
       fieldLabelModifier = f . drop 8
    }
    where
      f [] = []; f (x:xs) = toLower x : xs

-- | Invoices
-- Invoices are time-sensitive payment requests addressed to specific buyers. An invoice has a fixed price, typically denominated in fiat currency. It also has a BTC equivalent price, calculated by BitPay, with an expiration time of about 15 minutes.

newtype InvoiceId = InvoiceId { unInvoiceId :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)
-- newtype NotificationURL = NotificationURL { unNotificationURL :: Text } deriving (Show, Eq, ToJSON, FromJSON)
newtype RedirectURL = RedirectURL { unRedirectURL :: Text } deriving (Show, Eq, ToJSON, FromJSON)
newtype PaymentURL = PaymentURL { unPaymentURL :: Text } deriving (Show, Eq, ToJSON, FromJSON)
newtype POSData = POSData { unPOSData :: Text } deriving (Show, Eq, ToJSON, FromJSON)
newtype FullNotification = FullNotification { unFullNotification :: Bool } deriving (Show, Eq, ToJSON, FromJSON)
newtype Physical = Physical { unPhysical :: Text } deriving (Show, Eq, ToJSON, FromJSON)
newtype InvoiceStatus = InvoiceStatus { unInvoiceStatus :: Text } deriving (Show, Eq)

newtype BTCPrice = BTCPrice { unBTCPrice :: Integer } deriving (Show, Eq)
newtype Due = Due { unDue :: UTCTime } deriving (Show, Eq)
newtype PaidTime = PaidTime { unPaid :: UTCTime } deriving (Show, Eq)
newtype Time = Time { unTime :: UTCTime } deriving (Show, Eq)
newtype ExpirationTime = ExpirationTime { unExpirationTime :: UTCTime } deriving (Show, Eq)
newtype CurrentTime = CurrentTime { unCurrentTime :: UTCTime } deriving (Show, Eq)

-- | Invoice
data Invoice = Invoice {
      invoiceId :: InvoiceId
    , invoiceToken :: TokenId
    , invoicePrice :: Price
    , invoiceCurrency :: Currency
    , invoiceOrderId :: OrderId
    , invoiceItemDesc :: Description
    , invoiceItemCode :: Code
    , invoiceNotificationEmail :: Email
    , invoiceNotificationURL :: NotificationURL
    , invoiceRedirectURL :: RedirectURL
    , invoicePaymentUrls :: [PaymentURL]
    , invoicePosData :: POSData
    , invoiceTransactionSpeed :: TransactionSpeed
    , invoiceFullNotifications :: FullNotifications
    , invoicePhysical :: Physical
    , invoiceBuyer :: [Buyer]
    , invoiceUrl :: URL
    , invoiceStatus :: InvoiceStatus
    , invoiceBtcPrice :: BTCPrice -- ^ BTC equivalent of `price`, expires at `expirationTime`
    , invoiceBtcDue :: Due -- ^ BTC amount due
    , invoiceBtcPaid :: PaidTime -- ^ BTC amount paid
    , invoiceTime :: Time -- ^ UNIX time of invoice creation, in milliseconds
    , invoiceExpirationTime :: ExpirationTime
    , invoiceCurrentTime :: CurrentTime
    , invoiceExceptionStatus :: ExceptionStatus
    , invoiceRate :: Rate
    , invoiceExRates :: [ExRate]
    , invoiceTransaction :: [Transaction]
    , invoiceFlags :: Flag
} deriving (Show, Generic)

data ExceptionStatus = FalseStatus | PaidPartial | PaidOver
  deriving Show

newtype Refundable = Refundable { unRefundable :: Bool } deriving (Show, Eq)

data Flag = Flag {
    flagRefundable :: Refundable
  } deriving (Show, Eq, Generic)

newtype Amount = Amount { unAmount :: Integer } deriving (Show, Eq)
newtype Confirmations = Confirmations { unConfirmations :: Integer } deriving (Show, Eq)
newtype Locality = Locality { unLocality :: Text } deriving (Show, Eq)
newtype Region = Region { unRegion :: Text } deriving (Show, Eq)
newtype PostalCode = PostalCode { unPostalCode :: Text } deriving (Show, Eq)
newtype Notify = Notify { unNotify :: Bool } deriving (Show, Eq)
newtype TxType = TxType { unTxType :: Text } deriving (Show, Eq)

data Transaction = Transaction {
    transactionAmount :: Amount
  , transactionConfirmations :: Confirmations
  , transactionTime :: Time
  , transactionReceivedTime :: Time
  } deriving (Show, Eq, Generic)

-- | Buyer
data Buyer = Buyer {
    buyerName :: Name
  , buyerAddress1 :: AddressOne
  , buyerAddress2 :: AddressTwo
  , buyerLocality :: Locality
  , buyerRegion :: Region
  , buyerPostalCode :: PostalCode
  , buyerCountry :: Country
  , buyerEmail :: Email
  , buyerPhone :: Phone
  , buyerNotify :: Notify
  } deriving (Show, Eq, Generic)

newtype TimeStamp = TimeStamp { unTimeStamp :: UTCTime } deriving (Show, Eq)

data SourceType = SourceInvoice | SourceBitcoinTx deriving Show

newtype Scale = Scale { unScale :: Integer } deriving (Show, Eq)

newtype OrganizationNumericId =
  OrganizationNumericId { unOrganizationNumericId :: Text }
    deriving (Show, Eq)

-- | Ledger
data Ledger = Ledger {
    ledgerCode :: Code
  , ledgerTxType :: TxType -- type of sale?
  , ledgerSourceType :: SourceType
  , ledgerAmount :: Amount
  , ledgerTimestamp :: TimeStamp
  , ledgerDescription :: Description
  , ledgerScale :: Scale
  , ledgerInvoiceId :: InvoiceId
  , ledgerInvoiceAmount :: Amount
  , ledgerInvoiceCurrency :: Currency
  , ledgerExRate :: [ExRate]
  } deriving (Show, Generic)

-- All exchange rates used for invoice
newtype ExRate = ExRate {
    exRateCurrencyCode :: Code
  } deriving (Show, Eq, Generic)


-- | Organization
-- Organizations are entities such as businesses, schools, NGOs, and clubs. They may be affiliated with another organization.
newtype OrganizationId = OrganizationId { unOrganizationId :: Text } deriving (Show, Eq, ToJSON, FromJSON)
newtype OrganzationNumericId = OrganzationNumericId { unOrganzationNumericId :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

newtype AffiliateOID = AffiliateOID { unAffiliateOID :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)
newtype CompletedSetup = CompletedSetup { unCompletedSetup :: Bool } deriving (Show, Eq, ToJSON, FromJSON)

newtype Website = Website { unWebsite :: Text } deriving (Show, Eq)
newtype LegalBusinessName = LegalBusinessName { unLegalBusinessName :: Text } deriving (Show, Eq)

newtype IsNonProfit = IsNonProfit { unIsNonProfit :: Bool } deriving (Show, Eq)
newtype TaxID = TaxID { unTaxID :: Text } deriving (Show, Eq)
newtype GivenName = GivenName { unGivenName :: Text } deriving (Show, Eq)
newtype FamilyName = FamilyName { unFamilyName :: Text } deriving (Show, Eq)
newtype Percent = Percent { unPercent :: Integer } deriving (Show, Eq)
newtype Address = Address { unAddress :: Text } deriving (Show, Eq)
newtype IBAN = IBAN { unIBAN :: Text } deriving (Show, Eq)
newtype Swift = Swift { unSwift :: Text } deriving (Show, Eq)
newtype Sort = Sort { unSort :: Text } deriving (Show, Eq)

data Organization = Organization {
      organizationId :: OrganizationId
    , organizationNumericId :: OrganizationNumericId
    , organizationAffiliateOid :: AffiliateOID
    , organizationCompletedSetup :: CompletedSetup
    , organizationName :: Name
    , organizationAddress1 :: AddressOne
    , organizationAddress2 :: AddressTwo
    , organizationLocality :: Locality
    , organizationRegion :: Region
    , organizationPostalCode :: PostalCode
    , organizationCountry :: Country
    , organizationWebsite :: Website
    , organizationPhone :: Phone
    , organizationEmail :: Email
    , organizationLegalBusinessName :: LegalBusinessName
    , organizationIndustryCode :: IndustryCode
    , organizationIsNonProfit :: IsNonProfit
    , organizationTaxId :: TaxID
    , organizationOwners :: [Owner]
    , organizationPayoutInfo :: [PayoutInfo]
    , organizationNotificationURLs :: [NotificationURL]
  } deriving (Generic, Show, Eq)

-- | Business Owner
data Owner = Owner {
    ownerGivenName :: GivenName
  , ownerFamilyName :: FamilyName
  } deriving (Show, Eq, Generic)

newtype Routing = Routing { unRouting :: Text } deriving (Show, Eq, FromJSON, ToJSON)
newtype Account = Account { unAccount :: Text } deriving (Show, Eq, FromJSON, ToJSON)

data PayoutInfo = PayoutInfo {
    payoutInfoLabel :: Label
  , payoutInfoCurrency :: Currency
  , payoutInfoPercent :: Percent
  , payoutInfoBitcoinAddress :: Address
  , payoutInfoName :: Name
  , payoutInfoTaxId :: TaxID
  , payoutInfoRouting :: Routing
  , payoutInfoAccount :: Account -- ^ Bank account number
  , payoutInfoIban :: IBAN
  , payoutInfoSwift :: Swift
  , payoutInfoSort :: Sort -- ^ Bank sort code
  } deriving (Generic, Show, Eq)

newtype Format = Format { unFormat :: Text } deriving (Show, Eq, FromJSON, ToJSON)
newtype FailCount = FailCount { unFailCount :: Integer } deriving (Show, Eq, FromJSON, ToJSON)
newtype LastSuccess = LastSuccess { unLastSuccess :: UTCTime } deriving (Show, Eq)
newtype EventType = EventType { unEventType :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

data NotificationURL = NotificationURL {
    notificationURLUrl :: URL
  , notificationURLFormat :: Format
  , notificationURLEventTypes :: [EventType]
  , notificationURLFailCount :: FailCount
  , notificationURLLastSuccess :: LastSuccess
  } deriving (Show, Eq, Generic)

newtype PayoutId = PayoutId { unPayoutId :: Text } deriving (Show, Eq)

newtype PayoutAccount = PayoutAccount { unPayoutAccount :: Text } deriving (Show, Eq)
newtype PayoutStatus = PayoutStatus { unPayoutStatus :: Text } deriving (Show, Eq)
newtype BTC = BTC { unBTC :: Integer } deriving (Show, Eq)
newtype RequestDate = RequestDate { unRequestDate :: Text } deriving (Show, Eq)
newtype PayoutToken = PayoutToken { unPayoutToken :: Text } deriving (Show, Eq)
newtype EffectiveDate = EffectiveDate { unEffectiveDate :: UTCTime } deriving (Show, Eq)
newtype Reference = Reference { unReference :: Text } deriving (Show, Eq)
newtype PricingMethod = PricingMethod { unPricingMethod :: Text } deriving (Show, Eq)
newtype NotificationEmail = NotificationEmail { unNotificationEmail :: Text } deriving (Show, Eq)
newtype InstructionId = InstructionId { unInstructionId :: Text } deriving (Show, Eq)
newtype InstructionStatus = InstructionStatus { unInstructionStatus :: Text } deriving (Show, Eq)
newtype SettlementId = SettlementId { unSettlementId :: Text } deriving (Show, Eq)
newtype Status = Status { unStatus :: Text } deriving (Show, Eq)
newtype SubscriptionId = SubscriptionId { unSubscriptionId :: Text } deriving (Show, Eq)
newtype Schedule = Schedule { unSchedule :: Text } deriving (Show, Eq)
newtype NextDelivery = NextDelivery { unNextDeliver :: UTCTime } deriving (Show, Eq)
newtype TokenId = TokenId { unTokenId :: Text } deriving (Show, Eq, ToJSON, FromJSON)
newtype PairingCode = PairingCode { unPairingCode :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

-- | Payouts
-- Payouts are batches of bitcoin payments to employees, customers, partners, etc.
data Payout = Payout {
    payoutId :: PayoutId
  , payoutToken :: PayoutToken
  , payoutAccount :: PayoutAccount
  , payoutStatus :: PayoutStatus
  , payoutBtc :: BTC
  , payoutRequestDate :: RequestDate
  , payoutInstructions :: [Instruction]
  , payoutAmount :: Amount
  , payoutCurrency :: Currency
  , payoutEffectiveDate :: EffectiveDate
  , payoutReference :: Reference
  , payoutPricingMethod :: PricingMethod
  , payoutNotificationEmail :: NotificationEmail
  , payoutNotificationURL :: NotificationURL
  } deriving (Show, Eq, Generic)

data Instruction = Instruction {
    instructionId :: InstructionId
  , instructionStatus :: InstructionStatus
  , instructionBtc :: BTC
  , instructionTransactions :: [Transaction] -- [Text]
  , instructionAmount :: Amount
  , instructionAddress :: Address -- ^ Bitcoin address
  , instructionLabel :: Label -- ^ Label
  } deriving (Show, Eq, Generic)

-- | Rates
newtype RateName = RateName { unRateName :: Text } deriving (Show, Eq, ToJSON, FromJSON)
-- newtype Rate = Rate { unRate :: Integer } deriving (Show, Eq, ToJSON, FromJSON)
data Rate = Rate  {
    rateCode :: Code
  , rateName :: RateName
  , rateRate :: RateId
  } deriving (Show, Eq, Generic)

newtype RateId = RateId { unRateId :: Double }
  deriving (Show, Eq, FromJSON)

newtype CurrencyId = CurrencyId { unCurrencyId :: Text }
  deriving (Show, Eq, FromJSON, ToText, FromText)

newtype Rates = Rates { ratesData :: [Rate] } deriving (Generic, Show)
newtype SingleRate = SingleRate { singleRateData :: Rate } deriving (Generic, Show)

instance FromJSON SingleRate where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = map toLower . drop 10
    }

instance FromJSON Rates where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = map toLower . drop 5
    }

instance FromJSON Rate where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = map toLower . drop 4
    }

newtype Executed = Executed { unExecuted :: UTCTime }
  deriving (Show, Eq, ToJSON, FromJSON)

newtype Completed = Completed { unCompleted :: Bool }
  deriving (Show, Eq, ToJSON, FromJSON)

-- | Settlements
-- Settlements are transfers of payment profits from BitPay to bank accounts and bitcoin wallets owned by merchants, partners, etc. This endpoint exposes reports detailing these settlements.
data Settlement = Settlement
  { settlementId :: SettlementId -- ^ ResourceId
  , settlementAmount :: Amount -- ^ Settlement amount, in `currency`
  , settlementCurrency :: Currency -- ^ `BTC` or ISO 4217 3-character currency code
  , settlementStatus :: Status -- ^ Can be `processing`, `completed`, or `failed`
  , settlementDateCreated :: Created -- ^ UTC date, ISO-8601 format yyyy-mm-ddThh:mm:ssZ. Indicates date when settlement was created.
  , settlementDateExecuted :: Executed -- ^ UTC date, ISO-8601 format yyyy-mm-ddThh:mm:ssZ. Indicates date when settlement was executed. This is the time when ledger entries were written.
  , settlementDateCompleted :: Completed -- ^ UTC date, ISO-8601 format yyyy-mm-ddThh:mm:ssZ. Indicates date when settlement was completed. Present for `completed` and `failed` settlements only.
  } deriving (Show, Eq, Generic)

data Subscription = Subscription {
    subscriptionId :: SubscriptionId -- ^ Resource Id
  , subscriptionToken :: TokenId -- ^ API token for subscription resource
  , subscriptionBillData :: Bill -- ^ See `bills` resource
  , subscriptionSchedule :: Schedule -- ^ Schedule of repeat bill due dates. Can be `weekly`, `monthly`, `quarterly`, `yearly`, or a simple cron expression specifying seconds, minutes, hours, day of month, month, and day of week. BitPay maintains the difference between the due date and the delivery date in all subsequent, automatically-generated bills.
  , subscriptionStatus :: Status -- ^ Can be `draft` or `active` or `cancelled`. Subscriptions in active state will create new Bills on the nextDelivery date.
  , subscriptionNextDelivery :: NextDelivery -- ^ UTC date, ISO-8601 format yyyy-mm-dd or yyyy-mm-ddThh:mm:ssZ. Default is current time. Current or past date indicates that the bill can be delivered immediately. BitPay may modify the hh:mm:ss values in order to distribute deliveries evenly throughout the day.
  } deriving (Show, Generic)

data Facade = Merchant | POS
 deriving (Show, Generic, Eq, Ord)

instance FromJSON Facade where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = map toLower
    }
instance ToJSON Facade where
  toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = map toLower
    }

instance ToText StartDate where
  toText = T.pack . take 10 . show

instance ToText EndDate where
  toText = T.pack . take 10 . show

newtype StartDate = StartDate { unStartDate :: UTCTime } deriving (Show, Eq)
newtype EndDate = EndDate { unEndDate :: UTCTime } deriving (Show, Eq) 

instance ToJSON StartDate where toJSON = String . toText
instance ToJSON EndDate where toJSON = String . toText

data Token = Token {
    tokenId :: TokenId -- ^ Client identity based on bitcoin identity protocol
  , tokenPairingCode :: Maybe PairingCode -- ^ Access approval code
  , tokenFacade :: Maybe Facade -- ^ Can be `merchant`, or `pos`
  , tokenLabel :: Maybe Label -- ^ Token label, may include spaces, underscores, and dashes
  } deriving (Show, Generic) 

defToken :: TokenId -> Token
defToken t = Token t Nothing Nothing Nothing 

data TokenResponse = TokenResponse {
    trPolicies :: [Policy] -- * Policies
  , trResource :: Maybe ResourceId -- * Token Identifier
  , trToken :: TokenId -- * API token for token resource
  , trFacade :: Maybe Facade -- * Can be `merchant`, or `pos`
  , trDateCreated :: TokenCreated -- * UNIX time of creation, in milliseconds
  , trPairingExpiration :: PairingExpiration -- * UNIX time of expiration, in milliseconds
  , trPairingCode :: PairingCode -- * Access approval code
  } deriving (Show, Eq, Generic)

data Response = Response { rData :: [TokenResponse] } deriving (Generic, Show, Eq)

instance FromJSON Response where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = map toLower . drop 1
  }

instance ToJSON TokenResponse where
  toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = \(_:_:x:xs) -> toLower x : xs
  }

instance FromJSON TokenResponse where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = \(_:_:x:xs) -> toLower x : xs
    }

newtype ResourceId = ResourceId { unResourceId :: Text }
  deriving (Show, Eq, FromJSON, ToJSON)

newtype PairingExpiration =
  PairingExpiration { unPairingExpiration :: Integer }
    deriving (Show, Eq, FromJSON, ToJSON)

data Policy = Policy {
    pPolicy :: PolicyType
  , pMethod :: PolicyMethod
  , pParams :: Maybe [Text]
  } deriving (Show, Eq, Generic)

instance FromJSON Policy where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = map toLower . drop 1
  }

instance ToJSON Policy where
  toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = map toLower . drop 1
  }

data PolicyType = Sin | Access | Events | Id | Session
  deriving (Eq, Show, Generic)

instance FromJSON PolicyType where
  parseJSON = genericParseJSON defaultOptions {
    constructorTagModifier = map toLower
  }

instance ToJSON PolicyType where
  toJSON = genericToJSON defaultOptions {
    constructorTagModifier = map toLower
  }

instance ToJSON Token where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = map toLower . drop 5
  }

data PolicyMethod =
    RequireSin
  | RequireFacadeAccess
  | AllowEventStream
  | Invalidated
  | Inactive
  | Unclaimed
  | RequireSession
  deriving (Show, Eq, Generic)

--  `requireSin`, `requireFacadeAccess`, `allowEventStream`
--, `invalidated`, `inactive`, `unclaimed`, `requireSession`

instance FromJSON PolicyMethod where
  parseJSON = genericParseJSON defaultOptions {
    constructorTagModifier = \(x:xs) -> toLower x : xs
  }

instance ToJSON PolicyMethod where
  toJSON = genericToJSON defaultOptions {
    constructorTagModifier = \(x:xs) -> toLower x : xs
  }

-- | Industry codes used in `orgs` resource
data IndustryCode =
   BP1621 -- ^ Accounting
 | BP8293 -- ^ Airlines/Aviation
 | BP4443 -- ^ Alternative Dispute Resolution
 | BP4163 -- ^ Alternative Medicine
 | BP9372 -- ^ Animation
 | BP6770 -- ^ Apparel/Fashion
 | BP6823 -- ^ Architecture/Planning
 | BP3006 -- ^ Arts/Crafts
 | BP2459 -- ^ Automotive
 | BP0169 -- ^ Aviation/Aerospace
 | BP0683 -- ^ Banking/Mortgage
 | BP9810 -- ^ Biotechnology/Greentech
 | BP8822 -- ^ Bitcoin Mining Hardware
 | BP8452 -- ^ Bitcoin Mining Co-op
 | BP1627 -- ^ Broadcast Media
 | BP9648 -- ^ Building Materials
 | BP7908 -- ^ Business Supplies/Equipment
 | BP4175 -- ^ Capital Markets/Hedge Fund/Private Equity
 | BP1903 -- ^ Chemicals
 | BP2863 -- ^ Civic/Social Organization
 | BP6822 -- ^ Civil Engineering
 | BP1176 -- ^ Commercial Real Estate
 | BP0070 -- ^ Computer/Network Security
 | BP1060 -- ^ Computer Games
 | BP1683 -- ^ Computer Hardware
 | BP0135 -- ^ Computer Networking
 | BP4528 -- ^ Computer Software/Engineering
 | BP9823 -- ^ Construction
 | BP2835 -- ^ Consumer Electronics
 | BP0492 -- ^ Consumer Goods
 | BP5016 -- ^ Consumer Services
 | BP5268 -- ^ Cosmetics
 | BP5778 -- ^ Currency Exchange
 | BP3715 -- ^ Dairy
 | BP0793 -- ^ Defense/Space
 | BP8062 -- ^ Design
 | BP2962 -- ^ Dietary Supplements
 | BP8635 -- ^ Education Management
 | BP6150 -- ^ E-Learning
 | BP3759 -- ^ Electrical/Electronic Manufacturing
 | BP8424 -- ^ Entertainment
 | BP2769 -- ^ Entertainment - Adult
 | BP9312 -- ^ Environmental Services
 | BP9883 -- ^ Events Services
 | BP5020 -- ^ Executive Office
 | BP5203 -- ^ Facilities Services
 | BP8908 -- ^ Farming
 | BP1259 -- ^ Financial Services
 | BP0483 -- ^ Fine Art
 | BP2980 -- ^ Fishery
 | BP3789 -- ^ Food/Beverages
 | BP1083 -- ^ Food Production
 | BP2703 -- ^ Fund-Raising
 | BP8130 -- ^ Furniture
 | BP0428 -- ^ Gambling/Casinos
 | BP3492 -- ^ Glass/Ceramics/Concrete
 | BP3331 -- ^ Government Administration
 | BP5240 -- ^ Government Relations
 | BP2691 -- ^ Graphic Design/Web Design
 | BP2699 -- ^ Health/Fitness
 | BP6285 -- ^ Higher Education/Academia
 | BP6368 -- ^ Hospital/Health Care
 | BP5716 -- ^ Hospitality
 | BP7639 -- ^ Human Resources/HR
 | BP9442 -- ^ Import/Export
 | BP5884 -- ^ Individual/Family Services
 | BP0462 -- ^ Industrial Automation
 | BP9982 -- ^ Information Services
 | BP6246 -- ^ Information Technology/IT
 | BP7786 -- ^ Insurance
 | BP5144 -- ^ International Affairs
 | BP7474 -- ^ International Trade/Development
 | BP4945 -- ^ Internet
 | BP9235 -- ^ Investment Banking/Venture
 | BP6174 -- ^ Investment Management/Hedge Fund/Private Equity
 | BP5854 -- ^ Judiciary
 | BP5614 -- ^ Law Enforcement
 | BP0959 -- ^ Law Practice/Law Firms
 | BP0875 -- ^ Legal Services
 | BP0389 -- ^ Legislative Office
 | BP1152 -- ^ Leisure/Travel/Tourism
 | BP1671 -- ^ Libraries
 | BP0068 -- ^ Logistics/Supply Chain
 | BP2686 -- ^ Luxury Goods/Jewelry
 | BP8110 -- ^ Machinery
 | BP8883 -- ^ Management Consulting
 | BP6520 -- ^ Maritime
 | BP2641 -- ^ Market Research
 | BP2136 -- ^ Marketing/Advertising/Sales
 | BP5592 -- ^ Mechanical or Industrial Engineering
 | BP8891 -- ^ Media Production
 | BP4115 -- ^ Medical Devices/Equipment
 | BP2970 -- ^ Medical Practice
 | BP4178 -- ^ Mental Health Care
 | BP1043 -- ^ Military
 | BP4244 -- ^ Motion Pictures/Film
 | BP2215 -- ^ Museums/Institutions
 | BP0533 -- ^ Music
 | BP1664 -- ^ Nanotechnology
 | BP4929 -- ^ Newspapers/Journalism
 | BP5293 -- ^ Non-Profit/Volunteering
 | BP1226 -- ^ Novelties - Adult
 | BP1759 -- ^ Oil/Energy/Solar/Greentech
 | BP1376 -- ^ Online Publishing
 | BP5916 -- ^ Outsourcing/Offshoring
 | BP5206 -- ^ Package/Freight Delivery
 | BP6850 -- ^ Packaging/Containers
 | BP4340 -- ^ Paper/Forest Products
 | BP7841 -- ^ Performing Arts
 | BP7408 -- ^ Prepaid Cards/Gift Cards
 | BP7376 -- ^ Pharmaceuticals
 | BP8014 -- ^ Philanthropy
 | BP4387 -- ^ Photography
 | BP5475 -- ^ Plastics
 | BP2310 -- ^ Political Organization
 | BP0088 -- ^ Precious Metals
 | BP7297 -- ^ Primary/Secondary Education
 | BP1099 -- ^ Printing
 | BP5901 -- ^ Professional Training
 | BP9953 -- ^ Program Development
 | BP0937 -- ^ Public Policy
 | BP1761 -- ^ Public Relations/PR
 | BP7033 -- ^ Public Safety
 | BP3275 -- ^ Publishing Industry
 | BP1539 -- ^ Railroad Manufacture
 | BP8414 -- ^ Ranching
 | BP9091 -- ^ Real Estate/Mortgage
 | BP0007 -- ^ Recreational Facilities/Services
 | BP2864 -- ^ Religious Institutions
 | BP9264 -- ^ Renewables/Environment
 | BP6653 -- ^ Research Industry
 | BP2938 -- ^ Restaurants
 | BP6277 -- ^ Retail Industry
 | BP3520 -- ^ Security/Investigations
 | BP1590 -- ^ Semiconductors
 | BP1104 -- ^ Shipbuilding
 | BP7148 -- ^ Sporting Goods
 | BP3345 -- ^ Sports
 | BP3135 -- ^ Staffing/Recruiting
 | BP2620 -- ^ Supermarkets
 | BP6279 -- ^ Telecommunications
 | BP7010 -- ^ Textiles
 | BP5077 -- ^ Think Tanks
 | BP6741 -- ^ Tobacco
 | BP3367 -- ^ Translation/Localization
 | BP5132 -- ^ Transportation
 | BP4134 -- ^ Utilities
 | BP2191 -- ^ Venture Capital/VC
 | BP4737 -- ^ Veterinary
 | BP3527 -- ^ Video on Demand
 | BP7456 -- ^ Warehousing
 | BP0126 -- ^ Wholesale
 | BP3612 -- ^ Wine/Spirits
 | BP0514 -- ^ Wireless
 | BP8901 -- ^ Writing/Editing
   deriving (Show, Eq)

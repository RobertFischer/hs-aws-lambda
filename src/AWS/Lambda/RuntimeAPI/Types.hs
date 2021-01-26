module AWS.Lambda.RuntimeAPI.Types
		( LambdaInvocation(..)
		, LambdaResult(..)
		, MobileInvocationMetadata
		, Text
		, ct
		, module GHC.Generics
		, module Data.Aeson
		, module Data.Word
		, module Data.Function
		, module Control.Monad
		, module Data.Text.Conversions
		, module Data.String
		, module Control.Lens.Operators
		, module Data.Typeable
		, module Control.Exception.Safe
		, module Control.DeepSeq
		) where

import Control.Lens.Operators ( (^.) )
import Control.Monad ( void, forever )
import Data.Aeson (ToJSON, FromJSON, Options)
import Data.Function ( (&) )
import Data.String ( fromString )
import Data.Text (Text)
import Data.Text.Conversions ( ToText, FromText, convertText )
import Data.Word (Word64)
import GHC.Generics
import qualified Data.Aeson as JSON
import qualified Data.Char as Char
import Data.Typeable (typeOf)
import Control.Exception.Safe (throw, MonadThrow)
import Control.DeepSeq (NFData)

-- | Shorthand for 'convertText' from 'Data.Text.Conversions'
ct :: (ToText a, FromText b) => a -> b
ct = convertText

-- | Our implementation of how to map the field names to JSON labels
modifyFieldLabel :: String -> String
modifyFieldLabel str =
		if null trimmed then
			str
		else
			trimmed
	where
		trimmed = dropWhile Char.isLower str

jsonOptions :: Options
jsonOptions = JSON.defaultOptions
	{ JSON.fieldLabelModifier = modifyFieldLabel
	, JSON.omitNothingFields = True
	, JSON.unwrapUnaryRecords = True
	, JSON.tagSingleConstructors = False
	}

-- | Additional information available only when the Lambda is invoked through the
-- AWS Mobile SDK.  This data is currently unstructured, but will be updated to
-- be structured in some future major release.
--
-- (Pull requests very welcome.)
data MobileInvocationMetadata = MobileInvocationMetadata
	{ mimClientContext :: JSON.Value -- ^ the client's execution context
	, mimCognitoIdentity :: JSON.Value -- ^ the client's identity
	} deriving (Eq, Generic)

instance ToJSON MobileInvocationMetadata where
	toJSON = JSON.genericToJSON jsonOptions
	toEncoding = JSON.genericToEncoding jsonOptions

instance FromJSON MobileInvocationMetadata where
	parseJSON = JSON.genericParseJSON jsonOptions

-- | Represents the data provided to an invocation of the lambda
data LambdaInvocation payload = LambdaInvocation
	{ liAwsRequestId :: Text -- ^ The unique ID for this request
	, liDeadlineMs :: Word64 -- ^ The timetsamp of the deadline in Unix time
	, liInvokedFunctionArn :: Text -- ^ This function's ARN
	, liTraceId :: Text -- ^ The details about this AWS X-Ray trace
	, liMobileMetadata :: Maybe MobileInvocationMetadata -- ^ The mobile data if the Lambda was called from the AWS Mobile SDK
	, liPayload :: payload
	} deriving (Eq, Generic)

instance (ToJSON payload, Generic payload) => ToJSON (LambdaInvocation payload) where
	toJSON = JSON.genericToJSON jsonOptions
	toEncoding = JSON.genericToEncoding jsonOptions

instance (FromJSON payload, Generic payload) => FromJSON (LambdaInvocation payload) where
	parseJSON = JSON.genericParseJSON jsonOptions

type ErrorType = Text
type ErrorMessage = Text
type ErrorInfo = (ErrorType, ErrorMessage)

-- | The two possible results of a Lambda execution: success or failure
data LambdaResult payload
	= LambdaSuccess payload -- ^ Denotes success and provides the value to return
	| LambdaError ErrorInfo -- ^ Denotes failure and provides details
	| LambdaNop             -- ^ Denotes that no invocation was provided
	deriving (Eq, NFData, Generic)



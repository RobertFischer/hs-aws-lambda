module AWS.Lambda.RuntimeAPI.Types
		( LambdaInvocation(..)
		, LambdaResult(..)
		, LambdaExecutionContext(..)
		, MobileInvocationMetadata(..)
		, ErrorType
		, ErrorMessage
		, ErrorInfo
		) where

import           Control.DeepSeq     (NFData, NFData1)
import           Data.Aeson          (FromJSON, FromJSON1, Options, ToJSON,
                                      ToJSON1)
import qualified Data.Aeson          as JSON
import qualified Data.Char           as Char
import           Data.Text           (Text)
import           Data.Word           (Word64)
import           GHC.Generics
import qualified Network.HTTP.Client as HTTP

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
	{ mimClientContext :: Text  -- ^ the client's execution context
	, mimCognitoIdentity :: Text-- ^ the client's identity
	} deriving (Show, Eq, Generic, NFData)

instance ToJSON MobileInvocationMetadata where
	toJSON = JSON.genericToJSON jsonOptions
	toEncoding = JSON.genericToEncoding jsonOptions

instance FromJSON MobileInvocationMetadata where
	parseJSON = JSON.genericParseJSON jsonOptions

-- | Represents the data provided to an invocation of the lambda
data LambdaInvocation payload = LambdaInvocation
	{ liAwsRequestId :: Text      -- ^ The unique ID for this request
	, liDeadlineMs
		:: Word64 -- ^ The timetsamp of the deadline in Unix time
	, liInvokedFunctionArn :: Text-- ^ This function's ARN
	, liTraceId :: Text           -- ^ The details about this AWS X-Ray trace
	, liMobileMetadata
		:: Maybe MobileInvocationMetadata -- ^ The mobile data if the Lambda was called from the AWS Mobile SDK
	, liPayload
		:: payload -- ^ The input to the Lambda
	} deriving (Show, Eq, Generic, ToJSON, FromJSON, Generic1, ToJSON1, FromJSON1)

instance (NFData payload) => NFData (LambdaInvocation payload)
instance NFData1 LambdaInvocation

type ErrorType = Text
type ErrorMessage = Text
type ErrorInfo = (ErrorType, ErrorMessage)

-- | The two possible results of a Lambda execution: success or failure
data LambdaResult payload
	= LambdaSuccess payload -- ^ Denotes success and provides the value to return
	| LambdaError ErrorInfo -- ^ Denotes failure and provides details
	| LambdaNop             -- ^ Denotes that no invocation was provided
	deriving (Show, Eq, Generic, ToJSON, FromJSON, Generic1, ToJSON1, FromJSON1)

instance (NFData payload) => NFData (LambdaResult payload)
instance NFData1 LambdaResult

data LambdaExecutionContext a m b = LambdaExecutionContext
	{ lecApiPrefix
	  :: String
	, lecHttpManager :: HTTP.Manager
	, lecHandler
		:: LambdaInvocation a -> m (LambdaResult b)
	}

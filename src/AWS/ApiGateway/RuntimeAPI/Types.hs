module AWS.ApiGateway.RuntimeAPI.Types
		( ApiGatewayRequest(..)
		, ApiGatewayRequestContext(..)
		, ApiGatewayRequestHttpInfo(..)
		, agrBodyT
		, agrBodyJ
		, agrBodyBS
		, HeaderName(..)
		, mkHeaderName
		, ApiGatewayResponse(..)
		, ApiGatewayResponseBody(..)
		, mkApiGatewayResponse
		, mkApiGatewayResponseJSON
		, mkApiGatewayResponseStatus
		, ContentType
		) where

import           Control.DeepSeq            (NFData, NFData1)
import           Data.Aeson                 (FromJSON, FromJSON1, FromJSONKey,
                                             Options, ToJSON, ToJSON1,
                                             ToJSONKey)
import qualified Data.Aeson                 as JSON
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base64     as BSB64
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import qualified Data.Char                  as Char
import           Data.Map                   (Map)
import           Data.String                (IsString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding.Base64  as TB64
import           Data.Time.Clock.System     (SystemTime (..))
import           Data.Time.LocalTime        (ZonedTime (..))
import           Data.Word                  (Word64)
import           GHC.Generics
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Types         as HTTP

-- | Newtype wrapper around header names to control JSON serialization and work
-- with 'Text' instead of 'ByteString'.
newtype HeaderName = HeaderName (CI Text) deriving (Show, Eq, Generic, NFData, Ord, FromJSONKey, ToJSONKey)

-- | Converts an arbitrary text value into a 'HeaderName'.
mkHeaderName :: Text -> HeaderName
mkHeaderName = HeaderName . CI.mk

instance ToJSON HeaderName where
	toJSON (HeaderName t) = JSON.toJSON $ CI.foldedCase t

instance FromJSON HeaderName where
	parseJSON = fmap (HeaderName . CI.mk @Text) . JSON.parseJSON

-- | A Version 2.0 API Gateway Request payload
data ApiGatewayRequest = ApiGatewayRequest
	{ agrRouteKey :: Text
	, agrRawPath :: Text
	, agrRawQueryString :: Text
	, agrCookies
		:: [Text]
	, agrHeaders -- ^ Repeated headers have values combined with commas.
		:: Map HeaderName Text
	, agrQueryStringParameters
		:: Map Text Text
	, agrRequestContext
		:: ApiGatewayRequestContext
	, agrBody -- ^ See 'agrBodyT', 'agrBodyBS', and 'agrBodyJ'
		:: JSON.Value
	, agrPathParameters
		:: Map Text Text
	, agrStageVariables
		:: Map Text Text
	, agrIsBase64Encoded :: Bool
	} deriving (Show, Generic, NFData, FromJSON)

-- | Provides a 'ByteString' of the body.  If 'agrIsBase64Encoded' is 'True' and the
-- body is JSON text, then the body is decoded from Base64.  Otherwise, the JSON value
-- of the body is encoded to 'ByteString'.
agrBodyBS :: (MonadFail m) => ApiGatewayRequest -> m ByteString
agrBodyBS ApiGatewayRequest{agrBody,agrIsBase64Encoded} =
	if agrIsBase64Encoded then
		case agrBody of
			JSON.String t ->
				case BSB64.decodeBase64 . C8.pack $ T.unpack t of
					Left err -> fail $ T.unpack err
					Right value -> return value
			value ->
				return . C8L.toStrict $ JSON.encode value
	else
		return . C8L.toStrict $ JSON.encode agrBody

-- | Provides a 'Text' of the body, decoding
agrBodyT :: (MonadFail m) => ApiGatewayRequest -> m Text
agrBodyT ApiGatewayRequest{agrBody,agrIsBase64Encoded} = case agrBody of
	JSON.String t ->
		if agrIsBase64Encoded then
			case TB64.decodeBase64 t of
				Left err -> fail $ T.unpack err
				Right value -> return value
		else
			return t
	value -> return . T.pack . C8L.unpack $ JSON.encode value

-- | Attempts to decode the body value into an instance of 'FromJSON'.
agrBodyJ :: (MonadFail m, FromJSON a) => ApiGatewayRequest -> m a
agrBodyJ agr = agrBodyBS agr >>= \bodybs ->
		case JSON.eitherDecode' $ C8L.fromStrict bodybs of
			Left err -> fail err
			Right val -> return val

-- | The "requestContext" field of the API Gateway Request payload
data ApiGatewayRequestContext = ApiGatewayRequestContext
	{ agrcAccountId :: Text
	, agrcApiId :: Text
	, agrcDomainName :: Text
	, agrcDomainPrefix :: Text
	, agrcHttp
		:: ApiGatewayRequestHttpInfo
	, agrcRequestId :: Text
	, agrcRouteKey :: Text
	, agrcStage :: Text
	, agrcTime :: ZonedTime
	, agrcTimeEpoch :: SystemTime
	} deriving (Show, Generic, NFData)

instance ToJSON ApiGatewayRequestContext where
	toJSON = JSON.genericToJSON jsonOptions'
	toEncoding = JSON.genericToEncoding jsonOptions'

instance FromJSON ApiGatewayRequestContext where
	parseJSON = JSON.genericParseJSON jsonOptions'

-- | Our implementation of how to map the field names to JSON labels
modifyFieldLabel :: String -> String
modifyFieldLabel str =
	case dropWhile Char.isLower str of
		[] -> str
		a:rest -> Char.toLower a : rest

jsonOptions' :: Options
jsonOptions' = JSON.defaultOptions
	{ JSON.fieldLabelModifier = modifyFieldLabel
	, JSON.omitNothingFields = True
	, JSON.unwrapUnaryRecords = True
	, JSON.tagSingleConstructors = False
	}

data ApiGatewayResponse = ApiGatewayResponse
	{ agrsStatus
		:: HTTP.Status
	, agrsBody
		:: ApiGatewayResponseBody
	, agrsHeaders
		:: Map HeaderName Text
	, agrsCookies
		:: Map Text Text
	}

type ContentType = Text

-- | The different ways to express the API Gateway response body.
-- See also 'mkApiGatewayResponseJSON' and 'mkApiGatewayResponseStatus'.
data ApiGatewayResponseBody
	= BodyEmpty -- ^ Empty body
	| BodyByteString ContentType C8L.ByteString -- ^ Body will be Base64 encoded bytes
	| BodyText ContentType Text -- ^ Body will be passed along directly
	| BodyStatusMessage -- ^ Body will contain the message associated with the status code

mkApiGatewayResponse :: ApiGatewayResponseBody -> ApiGatewayResponse
-- ^ Creates an API Gateway response with the provided body and an "OK 200" status code.
mkApiGatewayResponse body = ApiGatewayResponse
		{ agrsCookies = mempty
		, agrsHeaders = [ ( mkHeaderName "content-type", derivedContentType ) ]
		, agrsBody = body
		, agrsStatus = HTTP.ok200
		}
	where
		derivedContentType = case body of
			BodyEmpty -> "text/plain"
			BodyByteString contentType _ -> contentType
			BodyText contentType _ -> contentType
			BodyStatusMessage -> "text/plain"


mkApiGatewayResponseJSON :: (ToJSON a) => a -> ApiGatewayResponse
-- ^ Creates an API Gateway response with the body set to the argument encoded as JSON.
mkApiGatewayResponseJSON a = mkApiGatewayResponse body
	where
		body = BodyByteString "application/json" $ JSON.encode a

mkApiGatewayResponseStatus :: HTTP.Status -> ApiGatewayResponse
-- ^ Creates an API Gateway response with the status set to the argument.
-- If the status is `204 No Content` ('HTTP.noContent204'), then the body
-- is set to 'BodyEmpty'; otherwise, the body is set to 'BodyStatusMessage'.
mkApiGatewayResponseStatus status = (mkApiGatewayResponse body) { agrsStatus = status }
	where
		body =
			if status == HTTP.noContent204 then
				BodyEmpty
			else
				BodyStatusMessage

data ApiGatewayRequestHttpInfo = ApiGatewayRequestHttpInfo
	{ agrhMethod :: Text
	, agrhPath :: Text
	, agrhProtocol :: Text
	, agrhSourceIp :: Text
	, agrhUserAgent :: Text
	} deriving (Generic, NFData, Eq, Show)

instance ToJSON ApiGatewayRequestHttpInfo where
	toJSON = JSON.genericToJSON jsonOptions'
	toEncoding = JSON.genericToEncoding jsonOptions'

instance FromJSON ApiGatewayRequestHttpInfo where
	parseJSON = JSON.genericParseJSON jsonOptions'

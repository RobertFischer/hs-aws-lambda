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
		, agrQueryParam
		, agrQueryParamExists
		, agrPathParam
		, agrPathParamExists
		, agrStageVar
		, agrStageVarExists
		, agrHeader
		, agrHeaderExists
		, ApiGatewayRequestAuth(..)
		, ApiGatewayRequestAuthJwt(..)
		) where

import           Control.DeepSeq            (NFData)
import           Data.Aeson                 (FromJSON, FromJSONKey, Options, ToJSON, ToJSONKey)
import qualified Data.Aeson                 as JSON
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base64     as BSB64
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import qualified Data.Char                  as Char
import           Data.Map                   (Map)
import qualified Data.Map as M
import           Data.String                (IsString(..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding.Base64  as TB64
import           Data.Time.Clock.System     (SystemTime (..))
import           Data.Time.LocalTime        (ZonedTime (..))
import           GHC.Generics
import qualified Network.HTTP.Types         as HTTP

-- | Newtype wrapper around header names to control JSON serialization and work
-- with 'Text' instead of 'ByteString'.
newtype HeaderName = HeaderName (CI Text) deriving (Show, Eq, Generic, NFData, Ord, FromJSONKey, ToJSONKey)

-- | Converts an arbitrary text value into a 'HeaderName'.
mkHeaderName :: Text -> HeaderName
mkHeaderName = HeaderName . CI.mk
{-# INLINE mkHeaderName #-}

instance IsString HeaderName where
	fromString = mkHeaderName . T.pack
	{-# INLINE fromString #-}

instance ToJSON HeaderName where
	toJSON (HeaderName t) = JSON.toJSON $ CI.foldedCase t
	{-# INLINE toJSON #-}

instance FromJSON HeaderName where
	parseJSON = fmap (HeaderName . CI.mk @Text) . JSON.parseJSON
	{-# INLINE parseJSON #-}

-- | A Version 2.0 API Gateway Proxy Request payload
data ApiGatewayRequest = ApiGatewayRequest
	{ agrVersion :: Text
	, agrRouteKey :: Text
	, agrRawPath :: Text
	, agrRawQueryString :: Text
	, agrCookies :: Maybe [Text]
	, agrHeaders :: Map HeaderName (Maybe Text) -- ^ Multiply-provided headers are intercalated with commas. See 'agrHeader', 'agrHeaderExists'
	, agrQueryStringParameters :: Maybe (Map Text (Maybe Text)) -- ^ See 'agrQueryParam', 'agrQueryParamExists'
	, agrBody :: Maybe Text -- ^ See 'agrBodyT', 'agrBodyBS', and 'agrBodyJ'
	, agrPathParameters :: Maybe (Map Text (Maybe Text)) -- ^ See 'agrPathParam', 'agrPathParamExists'
	, agrIsBase64Encoded :: Bool
	, agrStageVariables :: Maybe (Map Text (Maybe Text)) -- ^ See 'agrStageVar', 'agrStageVarExists'
	, agrRequestContext :: ApiGatewayRequestContext
	} deriving (Show, Generic, NFData, FromJSON)

-- | Checks for the existence of a request header
agrHeaderExists :: ApiGatewayRequest -> HeaderName -> Bool
agrHeaderExists ApiGatewayRequest{agrHeaders} key = M.member key agrHeaders

-- | Convenience accessor for request headers.  Note that it will fail if the 
--   specified key exists but its associated value is 'Nothing'.  If you just want
--   to check for a header's existence, use 'agrHeaderExists'
agrHeader :: (MonadFail m) => ApiGatewayRequest -> HeaderName -> m Text
agrHeader ApiGatewayRequest{agrHeaders} key = 
		case M.lookup key agrHeaders of 
			Nothing -> fail $ "Could not find key " <> show key <> " in request headers: " <> show agrHeaders
			Just maybeVal -> case maybeVal of 
				Nothing -> fail $ "No value provided for key " <> show key <> " in request headers: " <> show agrHeaders
				Just val -> return val

-- | Checks for the existence of a stage variable
agrStageVarExists :: ApiGatewayRequest -> Text -> Bool
agrStageVarExists ApiGatewayRequest{agrStageVariables} key = 
	maybe False (M.member key) agrStageVariables

-- | Convenience accessor for stage variables.  Note that it will fail if the 
--   specified key exists but its associated value is 'Nothing'.  If you just want
--   to check for parameter existence, use 'agrStageVarExists'
agrStageVar :: (MonadFail m) => ApiGatewayRequest -> Text -> m Text
agrStageVar ApiGatewayRequest{agrStageVariables} key = 
	case agrStageVariables of 
		Nothing -> fail "No stage variables provided"
		Just params -> case M.lookup key params of 
			Nothing -> fail $ "Could not find key " <> show key <> " in stage variables: " <> show params
			Just maybeVal -> case maybeVal of 
				Nothing -> fail $ "No value provided for key " <> show key <> " in stage variables: " <> show params
				Just val -> return val

-- | Checks for the existence of a path parameter
agrPathParamExists :: ApiGatewayRequest -> Text -> Bool
agrPathParamExists ApiGatewayRequest{agrPathParameters} key = 
	maybe False (M.member key) agrPathParameters

-- | Convenience accessor for path parameters.  Note that it will fail if the 
--   specified key exists but its associated value is 'Nothing'.  If you just want
--   to check for parameter existence, use 'agrPathParamExists'
agrPathParam :: (MonadFail m) => ApiGatewayRequest -> Text -> m Text
agrPathParam ApiGatewayRequest{agrPathParameters} key = 
	case agrPathParameters of 
		Nothing -> fail "No path parameters provided"
		Just params -> case M.lookup key params of 
			Nothing -> fail $ "Could not find key " <> show key <> " in path parameters: " <> show params
			Just maybeVal -> case maybeVal of 
				Nothing -> fail $ "No value provided for key " <> show key <> " in path parameters: " <> show params
				Just val -> return val

-- | Checks for the existence of a query parameter
agrQueryParamExists :: ApiGatewayRequest -> Text -> Bool
agrQueryParamExists ApiGatewayRequest{agrQueryStringParameters} key = 
	maybe False (M.member key) agrQueryStringParameters

-- | Convenience accessor for query parameters.  Note that it will fail if the 
--   specified key exists but its associated value is 'Nothing'.  If you just want
--   to check for parameter existence, use 'agrQueryParamExists'
agrQueryParam :: (MonadFail m) => ApiGatewayRequest -> Text -> m Text
agrQueryParam ApiGatewayRequest{agrQueryStringParameters} key = 
	case agrQueryStringParameters of 
		Nothing -> fail "No query string parameters provided"
		Just params -> case M.lookup key params of 
			Nothing -> fail $ "Could not find key " <> show key <> " in query string parameters: " <> show params
			Just maybeVal -> case maybeVal of 
				Nothing -> fail $ "No value provided for key " <> show key <> " in query string parameters: " <> show params
				Just val -> return val

-- | Provides a 'ByteString' of the body.  If 'agrIsBase64Encoded' is 'True' and the
-- body is JSON text, then the body is decoded from Base64.  Otherwise, the JSON value
-- of the body is encoded to 'ByteString'.
agrBodyBS :: (MonadFail m) => ApiGatewayRequest -> m ByteString
agrBodyBS ApiGatewayRequest{agrBody,agrIsBase64Encoded} =
	if agrIsBase64Encoded then
		case agrBody of
			Just t ->
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
	Just t ->
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

data ApiGatewayRequestAuthJwt = ApiGatewayRequestAuthJwt
	{ agrajClaims :: Map Text JSON.Value
	, agrajScopes :: [Text]
	} deriving (Show, Generic, NFData)

instance FromJSON ApiGatewayRequestAuthJwt where
	parseJSON = JSON.genericParseJSON jsonOptions'

newtype ApiGatewayRequestAuth = ApiGatewayRequestAuth
	{ agraJwt :: Maybe ApiGatewayRequestAuthJwt
	} deriving (Show, Generic, NFData)

instance FromJSON ApiGatewayRequestAuth where
	parseJSON = JSON.genericParseJSON jsonOptions'

-- | The "requestContext" field of the API Gateway Request payload
data ApiGatewayRequestContext = ApiGatewayRequestContext
	{ agrcAccountId :: Text
	, agrcApiId :: Text
	, agrcAuthorizer :: Maybe ApiGatewayRequestAuth
	, agrcDomainName :: Text
	, agrcDomainPrefix :: Text
	, agrcHttp :: ApiGatewayRequestHttpInfo
	, agrcRequestId :: Text
	, agrcRouteKey :: Text
	, agrcStage :: Text
	, agrcTime :: ZonedTime
	, agrcTimeEpoch :: SystemTime
	} deriving (Show, Generic, NFData)

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

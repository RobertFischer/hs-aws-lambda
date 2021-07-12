module AWS.Lambda.RuntimeAPI
	( runLambda
	) where

import           AWS.Lambda.RuntimeAPI.Types
import           Control.DeepSeq              (NFData)
import           Control.Exception.Safe       (MonadThrow)
import           Control.Monad                (forever, void)
import           Data.Aeson                   (FromJSON, ToJSON, eitherDecode',
                                               encode)
import qualified Data.ByteString.Char8        as C8
import qualified Data.CaseInsensitive         as CI
import           Data.List                    (find)
import qualified Data.Map.Strict              as Map
import           Data.String                  (IsString (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Typeable                (typeOf)
import qualified Network.HTTP.Client          as HTTP
import qualified Network.HTTP.Client.Internal as HTTP
import qualified Network.HTTP.Types.Header    as HTTP
import qualified Network.HTTP.Types.Status    as HTTP
import           System.Environment           (lookupEnv)
import           Text.Read                    (readMaybe)
import           UnliftIO

cts :: Text -> String
cts = Text.unpack

cst :: String -> Text
cst = Text.pack

mkLambdaHeaderName :: (CI.FoldCase s, Semigroup s, IsString s) => s -> CI.CI s
mkLambdaHeaderName str = CI.mk $ "Lambda-Runtime-" <> str

requestIdHeader :: HTTP.HeaderName
requestIdHeader = mkLambdaHeaderName "Aws-Request-Id"

invokedFunctionArnHeader :: HTTP.HeaderName
invokedFunctionArnHeader = mkLambdaHeaderName "Invoked-Function-Arn"

traceIdHeader :: HTTP.HeaderName
traceIdHeader = mkLambdaHeaderName "Trace-Id"

clientContextHeader :: HTTP.HeaderName
clientContextHeader = mkLambdaHeaderName "Client-Context"

cognitoIdentityHeader :: HTTP.HeaderName
cognitoIdentityHeader = mkLambdaHeaderName "Cognito-Identity"

unhandledErrorHeader :: HTTP.Header
unhandledErrorHeader = ( mkLambdaHeaderName "Function-Error-Type", "Unhandled" )

apiVersion :: String
apiVersion = "2018-06-01"

apiHostEnvVarName :: String
apiHostEnvVarName = "AWS_LAMBDA_RUNTIME_API"

-- | This function is intended to be your `main` implementation.  Given a handler for 'LambdaInvocation' instances,
--   it loops indefinitely (until AWS terminates the process) on the retrieval of invocations. It feeds each of those
--   invocations into the handler that was passed in as an argument. It then posts the result back to AWS and begins
--   the loop again.
runLambda :: (MonadUnliftIO m, MonadFail m, MonadThrow m, FromJSON a, ToJSON b, NFData a, NFData b) => (LambdaInvocation a -> m (LambdaResult b)) -> m ()
runLambda handler = do
		ctx <- lookupLEC handler
		forever $ doRound ctx

lookupLEC :: (MonadUnliftIO m, MonadFail m) => (LambdaInvocation a -> m (LambdaResult b)) -> m (LambdaExecutionContext a m b)
lookupLEC lecHandler = do
		apiHost <- lookupApiHost
		let lecApiPrefix = "http://" <> apiHost <> "/" <> apiVersion
		lecHttpManager <- liftIO $ HTTP.newManager managerSettings
		return LambdaExecutionContext{..}
	where
		lookupApiHost = liftIO ( lookupEnv apiHostEnvVarName ) >>= \case
			Nothing -> fail $ "No API host environment variable name found: " <> apiHostEnvVarName
			Just val -> return val
		managerSettings = HTTP.defaultManagerSettings
			{ HTTP.managerResponseTimeout = HTTP.responseTimeoutNone
			, HTTP.managerIdleConnectionCount = 1
			}

doRound :: (MonadUnliftIO m, MonadThrow m, MonadFail m, FromJSON a, ToJSON b, NFData a, NFData b) => LambdaExecutionContext a m b -> m ()
doRound ctx = getNextInvocation >>= \case
			Nothing -> return ()
			Just request -> processRequest request >>= postResult ctx request
	where
		getNextInvocation = handleAnyDeep handleTopException ( Just <$> fetchNext ctx )
		handler = lecHandler ctx
		processRequest invoc = handleAnyDeep handleException $ handler invoc
		handleException e = return $ LambdaError (cst $ exToTypeStr e, cst $ exToHumanStr e)
		handleTopException err = handleInvocationException ctx err >> return Nothing

exToTypeStr :: (Exception e) => e -> String
exToTypeStr = show . typeOf

exToHumanStr :: (Exception e) => e -> String
exToHumanStr = displayException

handleInvocationException :: (MonadIO m, Exception e) => LambdaExecutionContext a m b -> e -> m ()
handleInvocationException
	LambdaExecutionContext{lecApiPrefix, lecHttpManager}
	err
	= liftIO $ do
		putStrLn "!!!Invocation Exception!!!"
		putStrLn "\tThis is a problem with the Lambda Runtime API or AWS."
		putStrLn "\tThe problem is not in your code. Scroll down for details."
		putStrLn "\tThe Runtime API will now attempt to get another invocation."
		putStrLn ""
		printTypeStr
		printHumanStr
		putStrLn "^^^Invocation Exception^^^"
		initReq <- makeHttpRequest "POST" url body
		let req = initReq
			{ HTTP.requestHeaders = unhandledErrorHeader : HTTP.requestHeaders initReq
			}
		response <- HTTP.httpNoBody req lecHttpManager
		checkResponseStatus response
	where
		url = lecApiPrefix <> "/runtime/init/error"
		body = Just $ Map.fromList
			[ ( "errorMessage", humanStr )
			, ( "errorType", typeStr )
			]
		typeStr = exToTypeStr err
		humanStr = exToHumanStr err
		printTypeStr = putStrLn typeStr
		printHumanStr =
			if typeStr == humanStr then
				return ()
			else
				putStrLn $ exToHumanStr err

checkResponseStatus :: MonadFail m => HTTP.Response body -> m ()
checkResponseStatus response =
	let status = HTTP.responseStatus response in
	if HTTP.statusIsSuccessful $ HTTP.responseStatus response then
		return ()
	else
		fail $ "Received non-successful status when trying to fetch: " <> show status

fetchNext :: (MonadUnliftIO m, MonadThrow m, MonadFail m, FromJSON a) => LambdaExecutionContext a m b ->  m (LambdaInvocation a)
fetchNext LambdaExecutionContext{lecApiPrefix, lecHttpManager} = do
		req <- makeHttpRequest "GET" url (Nothing::Maybe ())
		response <- liftIO $ HTTP.httpLbs req lecHttpManager
		checkResponseStatus response
		liPayload <- readPayload response
		liAwsRequestId <- cst <$> readHeader response requestIdHeader
		liDeadlineMs <- readDeadline response
		liInvokedFunctionArn <- cst <$> readHeader response invokedFunctionArnHeader
		liTraceId <- cst <$> readHeader response traceIdHeader
		let liMobileMetadata = readMobileMetadata response
		return $ LambdaInvocation{..}
	where
		url = lecApiPrefix <> "/runtime/invocation/next"
		readMobileMetadata response = do
			mimClientContext <- cst <$> readHeader response clientContextHeader
			mimCognitoIdentity <- cst <$> readHeader response cognitoIdentityHeader
			return $ MobileInvocationMetadata{..}
		readHeader response headerName =
			let headers = HTTP.responseHeaders response in
			let finder (otherHeaderName, _) = headerName == otherHeaderName in
			case find finder headers of
				Just (_,value) -> return $ C8.unpack value
				Nothing -> fail $ "Could not find header: " <> show headerName
		readDeadline response = do
			headerValue <- readHeader response "Lambda-Runtime-Deadline-Ms"
			case readMaybe headerValue of
				Nothing -> fail $ "Could not parse deadline header value: " <> headerValue
				Just parsed -> return parsed
		readPayload response =
			let body = HTTP.responseBody response in
			case eitherDecode' body of
				Left err -> fail $ "Could not parse the body of the next invocation: " <> err
				Right value -> return value

makeHttpRequest :: (MonadThrow m, ToJSON b) => String -> String -> Maybe b -> m HTTP.Request
makeHttpRequest method url maybeBody =
		customizeRequest <$> HTTP.parseUrlThrow (method <> " " <> url)
	where
		customizeRequest initReq = initReq
			{ HTTP.decompress = HTTP.alwaysDecompress
			, HTTP.requestHeaders =
				[ ( HTTP.hAccept, "application/json" )
				, ( HTTP.hContentType, "application/json" )
				]
			, HTTP.requestBody = HTTP.RequestBodyLBS $ maybe "" encode maybeBody
			}

postResult :: (MonadUnliftIO m, MonadThrow m, ToJSON b) => LambdaExecutionContext a m b -> LambdaInvocation a -> LambdaResult b -> m ()
postResult
	LambdaExecutionContext{lecApiPrefix, lecHttpManager}
	LambdaInvocation{liAwsRequestId, liTraceId}
	result =
		case result of
			LambdaNop -> return ()
			(LambdaSuccess payload) -> do
				let url = lecApiPrefix <> "/runtime/invocation/" <> cts liAwsRequestId <> "/response"
				let body = Just payload
				req <- addTraceId <$> makeHttpRequest "POST" url body
				void . liftIO $ HTTP.httpNoBody req lecHttpManager
			LambdaError (errType, errMsg) -> do
				let url = lecApiPrefix <> "/runtime/invocation/" <> cts liAwsRequestId <> "/error"
				let body = Just $ Map.fromList
					[ ( "errorMessage", errMsg )
					, ( "errorType", errType )
					]
				req <- makeHttpRequest "POST" url body
				void . liftIO $ HTTP.httpNoBody req lecHttpManager
	where
		addTraceId req = req
			{ HTTP.requestHeaders
				= (traceIdHeader, C8.pack $ cts liTraceId) :  HTTP.requestHeaders req
			}

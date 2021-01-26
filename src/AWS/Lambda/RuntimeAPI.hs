module AWS.Lambda.RuntimeAPI
	( runLambda
	) where

import AWS.Lambda.RuntimeAPI.Types
import UnliftIO.Exception
import UnliftIO
import Network.HTTP.Simple
import System.Environment ( lookupEnv )
import qualified Data.Map.Strict as Map

apiVersion :: String
apiVersion = "2018-06-01"

apiHostEnvVarName :: String
apiHostEnvVarName = "AWS_LAMBDA_RUNTIME_API"

-- | This function is intended to be your `main` implementation.  Given a handler for 'LambdaInvocation' instances,
--   it loops indefinitely (until AWS terminates the process) on the retrieval of invocations. It feeds each of those
--   invocations into the handler that was passed in as an argument. It then posts the result back to AWS and begins
--   the loop again.
runLambda :: (MonadUnliftIO m, MonadFail m, MonadThrow m, Generic a, FromJSON a, ToJSON b, NFData b) => (LambdaInvocation a -> m (LambdaResult b)) -> m ()
runLambda handler = do
		apiHost <- lookupApiHost
		let apiPrefix = "http://" <> apiHost <> "/" <> apiVersion
		forever $ doRound apiPrefix handler
	where
		lookupApiHost = liftIO ( lookupEnv apiHostEnvVarName ) >>= \case
			Nothing -> fail $ "No API host environment variable name found: " <> apiHostEnvVarName
			Just val -> return val

doRound :: (MonadUnliftIO m, MonadThrow m, Generic a, FromJSON a, ToJSON b, NFData b) => String -> (LambdaInvocation a -> m (LambdaResult b)) -> m ()
doRound apiPrefix handler = handleAnyDeep handleTopException $ do
		invocation <- fetchNext apiPrefix
		result <- processRequest invocation
		postResult apiPrefix invocation result
	where
		processRequest invoc = handleAnyDeep handleException $ handler invoc
		handleException e = return $ LambdaError (ct $ exToTypeStr e, ct $ exToHumanStr e)
		handleTopException = handleInvocationException apiPrefix

exToTypeStr :: (Exception e) => e -> String
exToTypeStr = show . typeOf

exToHumanStr :: (Exception e) => e -> String
exToHumanStr = displayException

handleInvocationException :: (MonadIO m, MonadThrow m, Exception e) => String -> e -> m ()
handleInvocationException apiPrefix err = do
		liftIO $ do
			putStrLn "!!!Invocation Exception!!!"
			putStrLn "\tThis is a problem with the Lambda Runtime API or AWS."
			putStrLn "\tThe problem is not in your code."
			putStrLn "\tThe Runtime API will now attempt to get another invocation."
			putStrLn ""
			printTypeStr
			printHumanStr
			putStrLn "^^^Invocation Exception^^^"
		initReq <- parseRequest $ apiPrefix <> "/runtime/init/error"
		let req = initReq
			& setRequestMethod "POST"
			& setRequestBodyJSON (
				Map.fromList
					[ ( "errorMessage", humanStr )
					, ( "errorType", typeStr )
					]
				)
			& addRequestHeader "Lambda-Runtime-Function-Error-Type" "Unhandled"
		void $ httpNoBody req
	where
		typeStr = exToTypeStr err
		humanStr = exToHumanStr err
		printTypeStr = putStrLn typeStr
		printHumanStr =
			if typeStr == humanStr then
				return ()
			else
				putStrLn $ exToHumanStr err

fetchNext :: (MonadUnliftIO m, MonadThrow m, FromJSON a, Generic a) => String ->  m (LambdaInvocation a)
fetchNext apiPrefix = do
	response <- httpJSONEither fetchNextReq
	case getResponseBody response of
		(Left ex) -> throw ex
		(Right invoc) -> return invoc
	where
		fetchNextReq::Request = fromString $ apiPrefix <> "/runtime/invocation/next"

postResult :: (MonadUnliftIO m, MonadThrow m, ToJSON b) => String -> LambdaInvocation a -> LambdaResult b -> m ()
postResult _ _ LambdaNop = return ()
postResult apiPrefix LambdaInvocation{liAwsRequestId} (LambdaSuccess payload) = do
	initReq <- parseRequest $ apiPrefix <> "/" <> ct liAwsRequestId <> "/response"
	void . httpNoBody $ initReq
		& setRequestMethod "Post"
		& setRequestBodyJSON payload
postResult apiPrefix LambdaInvocation{liAwsRequestId} (LambdaError (errType, errMsg)) = do
		initReq <- parseRequest $ apiPrefix <> "/" <> ct liAwsRequestId <> "/error"
		let req = initReq
			& setRequestMethod "POST"
			& setRequestBodyJSON reqBody
		void $ httpNoBody req
	where
		reqBody = Map.fromList
			[ ( "errorMessage", errMsg )
			, ( "errorType", errType )
			]

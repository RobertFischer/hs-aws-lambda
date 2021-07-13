module AWS.ApiGateway.RuntimeAPI
	( runLambda
	, module AWS.ApiGateway.RuntimeAPI.Types
	, module AWS.Lambda.RuntimeAPI.Types
	) where

import           AWS.ApiGateway.RuntimeAPI.Types
import qualified AWS.Lambda.RuntimeAPI           as Lambda
import           AWS.Lambda.RuntimeAPI.Types
import           Control.DeepSeq                 (NFData)
import           Control.Exception.Safe          (MonadThrow)
import           Control.Monad                   (forever, void)
import           Data.Aeson                      (FromJSON, ToJSON,
                                                  eitherDecode', encode)
import qualified Data.Aeson                      as JSON
import           Data.ByteString.Base64          (encodeBase64)
import qualified Data.ByteString.Char8           as C8
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.CaseInsensitive            as CI
import           Data.List                       (find)
import qualified Data.Map.Strict                 as Map
import           Data.String                     (IsString (..))
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Typeable                   (typeOf)
import qualified Data.Vector                     as V
import qualified Network.HTTP.Client             as HTTP
import qualified Network.HTTP.Client.Internal    as HTTP
import qualified Network.HTTP.Types.Header       as HTTP
import qualified Network.HTTP.Types.Status       as HTTP
import           System.Environment              (lookupEnv)
import           Text.Read                       (readMaybe)
import           UnliftIO

-- | This function is intended to be your `main` implementation.  Given a handler for 'ApiGatewayRequest' instances,
--   it loops indefinitely (until AWS terminates the process) on the retrieval of invocations. It feeds each of those
--   invocations into the handler that was passed in as an argument. It then posts the result back to AWS and begins
--   the loop again.
runLambda :: (MonadUnliftIO m, MonadFail m, MonadThrow m) =>
	(LambdaInvocation ApiGatewayRequest -> m ApiGatewayResponse) -> m ()
runLambda impl = Lambda.runLambda $ handler impl

handler :: (MonadUnliftIO m) =>
	(LambdaInvocation ApiGatewayRequest -> m ApiGatewayResponse) ->
	LambdaInvocation ApiGatewayRequest -> m (LambdaResult JSON.Value)
handler impl invocation = toLambdaResponse <$> impl invocation

toLambdaResponse :: ApiGatewayResponse -> LambdaResult JSON.Value
toLambdaResponse ApiGatewayResponse{agrsStatus,agrsBody,agrsCookies,agrsHeaders} =
		response $ case agrsBody of
			BodyEmpty -> (False, "")
			BodyText _ t -> (False, t)
			BodyByteString _ bs -> (True, encodeBase64 $ LBS.toStrict bs)
			BodyStatusMessage -> (True, encodeBase64 $ HTTP.statusMessage agrsStatus)
	where
		response (isB64,bodyText)
				= LambdaSuccess $ JSON.Object
					[ ("cookies", JSON.toJSON agrsCookies)
					, ("isBase64Encoded", JSON.toJSON isB64)
					, ("statusCode", JSON.toJSON $ HTTP.statusCode agrsStatus)
					, ("headers", JSON.toJSON agrsHeaders)
					, ("body", JSON.toJSON bodyText)
					]

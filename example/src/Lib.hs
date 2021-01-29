module Lib
    ( someFunc
    ) where

import AWS.Lambda.RuntimeAPI ( runLambda )
import AWS.Lambda.RuntimeAPI.Types ( LambdaResult(..), LambdaInvocation(..) )
import Data.Aeson ( encode, Value )
import qualified Data.ByteString.Lazy.Char8 as C8

someFunc :: IO ()
someFunc = runLambda handler

handler :: LambdaInvocation Value -> IO (LambdaResult String)
handler request = do
    putStrLn . C8.unpack $ encode request
    return $ LambdaSuccess "This is the result payload"

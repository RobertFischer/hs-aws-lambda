# The Library for Easily Writing Your Lambdas in Haskell

Historically, if you wanted to use Haskell to write your Lambdas, you had to use
[a custom runtime](https://theam.github.io/aws-lambda-haskell-runtime/).  However,
[AWS now allows you to use Docker images instead](https://aws.amazon.com/blogs/aws/new-for-aws-lambda-container-image-support/),
which simplifies deployments and
[avoids some nasty problems](https://theam.github.io/aws-lambda-haskell-runtime/05-common-errors.html).

This library provides the translation layer that you need in order to fully leverage Lambda. It is implemented
in a classy way for maximum flexibility, even if it makes the type signatures a bit complicated.  There is an
example of an implementation in the `example` folder, including a demonstration of how to do deployment.
Here's the simplest usage, adapted slightly from the example:

```haskell

import AWS.Lambda.RuntimeAPI ( runLambda )
import AWS.Lambda.RuntimeAPI.Types ( LambdaResult(..), LambdaInvocation(..) )
import Data.Aeson ( encode, Value )
import qualified Data.ByteString.Lazy.Char8 as C8

-- Two lines of boilerplate
main :: IO ()
main = runLambda handler

-- Your implementation's code
handler :: LambdaInvocation Value -> IO (LambdaResult String)
handler request = do
	putStrLn . C8.unpack $ encode request
	return $ LambdaSuccess "This is the result payload"

```

The request payload is wrapped in a `LambdaInvocation` type, which provides access to the
particular invocation that is being processed.  In this case, we're treating the type as an
Aeson `Value`, which represents any arbitrary JSON structure.

Your handler is responsible for consuming that request and producing a `LambdaResult`. A
`LambdaResult` instance is usually either `LambdaSuccess payload` or `LambdaError (Text, Text)`.
In the case of `LambdaSuccess`, the payload defines the "result" of the Lambda, and will be
transformed into JSON via the `ToJSON` typeclass.  In the case of
`LambdaError`, the payload is `(ErrorType, ErrorMessage)`, which is passed to the Lambda
Runtime API in order to give a meaningful error to the user.  There is also `LambdaNop`
which does nothing, but that's intended for internal use only.

All of this executes in the `IO` monad in the example above, but you can use any
monad that implements `MonadUnliftIO`, `MonadFail`, and `MonadThrow`.

And that's pretty much it.

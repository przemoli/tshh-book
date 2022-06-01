module Docker where

import RIO
import qualified Network.HTTP.Simple as HTTP
import qualified Data.Aeson as Aeson
import qualified Socket

data CreateContainerOptions
    = CreateContainerOptions
        { image :: Image
        }

newtype Image = Image Text
    deriving (Eq, Show)

imageToText :: Image -> Text
imageToText (Image step) = step

createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
    manager <- Socket.newManager "/var/run/docker.sock"
    let body = Aeson.Null -- TODO figure out actual request body
    let req = HTTP.defaultRequest
            & HTTP.setRequestManager manager
            & HTTP.setRequestPath "/v1.40/containers/create"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body
    res <- HTTP.httpBS req
    -- Dump the response to stdout to check what we' are getting back
    traceShowIO res

newtype ContainerExitCode = ContainerExitCode Int
    deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code
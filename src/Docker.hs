module Docker where

import RIO

data CreateContainerOptions
    = CreateContainerOptions
        { image :: Image
        }

newtype Image = Image Text
    deriving (Eq, Show)

imageToText :: Image -> Text
imageToText (Image step) = step

createContainer :: CreateContainerOptions -> IO ()
createContainer options = undefined

newtype ContainerExitCode = ContainerExitCode Int
    deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code
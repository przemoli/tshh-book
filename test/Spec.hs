module Main where

import RIO
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import Core

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands
    = Step
        { name = StepName name
        , image = Image image
        , commands = NonEmpty.Partial.fromList commands
        }

makePipeline :: [Step] -> Pipeline
makePipeline steps =
    Pipeline { steps = NonEmpty.Partial.fromList steps }

testPipeline :: Pipeline
testPipeline = makePipeline
    [ makeStep "First step" "ubuntu" ["date"]
    , makeStep "Second step" "ubuntu" ["uname -r"]
    ]

testBuild :: Build
testBuild = Build
    { pipeline = testPipeline
    , state = BuildReady
    }


main :: IO ()
main = pure ()
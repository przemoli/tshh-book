module Main where

import RIO
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import Core
import Docker
import Runner
import Test.Hspec
import qualified RIO.Map as Map
import qualified System.Process.Typed as Process

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands
    = Step
        { name = StepName name
        , image = Docker.Image image
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

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
    build <- runner.prepareBuild $ makePipeline
        [ makeStep "First step" "ubuntu" ["date"]
        , makeStep "Second step" "ubuntu" ["uname -r"]
        ]
    result <- runner.runBuild build
    result.state `shouldBe` BuildFinished BuildSucceeded
    Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
    build <- runner.prepareBuild $ makePipeline
        [ makeStep "Should fail" "ubuntu" ["exit 1"]
        ]
    result <- runner.runBuild build
    result.state `shouldBe` BuildFinished BuildFailed
    Map.elems result.completedSteps `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

cleanupDocker :: IO ()
cleanupDocker = void do
    Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=quad\")"



main :: IO ()
main = hspec do
    docker <- runIO Docker.createService
    runner <- runIO $ Runner.createService docker
    beforeAll cleanupDocker $ describe "Quad CI" do
        it "should run a build (success)" do
            testRunSuccess runner
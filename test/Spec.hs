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

testCreateContainer :: Docker.Service -> IO ()
testCreateContainer docker = do
    let options = CreateContainerOptions
                { image = Image "ubuntu"
                , script = "exit 1"
                , volume = Volume "testCreateContainer"
                }
    containerId <- docker.createContainer options
    status <- docker.containerStatus containerId
    status `shouldBe` ContainerOther "created"

testStartContainer :: Docker.Service -> IO ()
testStartContainer docker = do
    let options = CreateContainerOptions
                { image = Image "ubuntu"
                , script = "exit 1"
                , volume = Volume "testStartContainer"
                }
    containerId <- docker.createContainer options
    status <- docker.containerStatus containerId
    status `shouldBe` ContainerOther "created"
    docker.startContainer containerId
    status2 <- docker.containerStatus containerId
    status2 `shouldBe` ContainerRunning

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

testSharedWorkspace :: Docker.Service -> Runner.Service -> IO ()
testSharedWorkspace docker runner = do
    build <- runner.prepareBuild $ makePipeline
                [ makeStep "Create file" "ubuntu" ["echo hello > test"]
                , makeStep "Read file" "ubuntu" ["cat test"]
                ]
    result <- runner.runBuild build
    result.state `shouldBe` BuildFinished BuildSucceeded
    Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

cleanupDocker :: IO ()
cleanupDocker = void do
    Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=quad\")"



main :: IO ()
main = hspec do
    docker <- runIO Docker.createService
    runner <- runIO $ Runner.createService docker
    beforeAll cleanupDocker $ describe "Quad CI" do
        it "should create a container" do
            testCreateContainer docker
        it "should start a container" do
            testStartContainer docker
        it "should run a build (success)" do
            testRunSuccess runner
        it "should run a build (failure)" do
            testRunFailure runner
        it "shoudl share workspace between steps" do
            testSharedWorkspace docker runner
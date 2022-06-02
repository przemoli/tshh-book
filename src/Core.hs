module Core where

import RIO
import qualified RIO.Map as Map
import qualified RIO.List as List
import qualified Docker as Docker
import qualified RIO.NonEmpty as NonEmpty
import qualified RIO.Text as Text

data Pipeline
    = Pipeline
        { steps :: NonEmpty Step
        }
    deriving (Eq, Show)

data Step
    = Step
        { name :: StepName
        , commands :: NonEmpty Text
        , image :: Docker.Image
        }
    deriving (Eq, Show)

data StepResult
    = StepFailed Docker.ContainerExitCode
    | StepSucceeded
    deriving (Eq, Show)

exitCodeToStepResults :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResults exit =
    if Docker.exitCodeToInt exit == 0
        then StepSucceeded
        else StepFailed exit

data Build
    = Build
        { pipeline :: Pipeline
        , state :: BuildState
        , completedSteps :: Map StepName StepResult
        }
    deriving (Eq, Show)

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build = 
    if allSucceeded
        then case nextStep of
            Just step -> Right step
            Nothing -> Left BuildSucceeded
        else Left BuildFailed
    where
        allSucceeded = List.all ((==) StepSucceeded) build.completedSteps
        nextStep = List.find f build.pipeline.steps
        f step = not $ Map.member step.name build.completedSteps

data BuildState
    = BuildReady
    | BuildRunning BuildRunningState
    | BuildFinished BuildResult
    deriving (Eq, Show)

data BuildRunningState
    = BuildRunningState
    { step :: StepName
    , container :: Docker.ContainerId
    }
    deriving (Eq, Show)

data BuildResult
    = BuildSucceeded
    | BuildFailed
    | BuildUnexpectedState Text
    deriving (Eq, Show)

newtype StepName = StepName Text
    deriving (Eq, Show, Ord)


progress :: Docker.Service -> Build -> IO Build
progress docker build =
    case build.state of
        BuildReady -> 
            case buildHasNextStep build of
                Left result ->
                    pure $ build{state = BuildFinished result}
                Right step -> do
                    let script = Text.unlines $
                            ["set -ex"] <> NonEmpty.toList step.commands
                    let options = Docker.CreateContainerOptions step.image script
                    container <- docker.createContainer options
                    docker.startContainer container
                    let s = BuildRunningState 
                            { step = step.name
                            , container = container
                            }
                    pure $ build{state = BuildRunning s}
        BuildRunning state -> do
            status <- docker.containerStatus state.container
            case status of
                Docker.ContainerRunning ->
                    pure build
                Docker.ContainerExited exit -> do
                    let result = exitCodeToStepResults exit
                    pure build
                        { completedSteps
                            = Map.insert state.step result build.completedSteps
                        , state = BuildReady
                        }
                Docker.ContainerOther other -> do
                    let s = BuildUnexpectedState other
                    pure build{state = BuildFinished s}
        BuildFinished _ -> pure build

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step
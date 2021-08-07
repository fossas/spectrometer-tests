module Repo
  ( repo,
    Repo (..),
    Analysis (..),
    TestProject (..),
    TestC,
    simpleTestProject,
  )
where

import Control.Carrier.Diagnostics
import Control.Carrier.Exec.NixShell
import Control.Carrier.Finally
import Data.Foldable (traverse_, for_)
import Data.Text (Text)
import Data.String.Conversion (toText)
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Graphing (Graphing)
import Path
import qualified Path.IO as PIO
import Test.Hspec
import Types (DiscoveredProject(..), Dependency, FoundTargets, GraphBreadth)

data Repo a = Repo
  { -- | Path to the root of the repo
    repoRoot :: Path Rel Dir,
    -- | An optional path to a build script required for the analysis strategies to succeed.
    -- The script is run with @repoRoot@ as the workdir.
    repoPrebuildScript :: Maybe (Path Rel File),
    -- | A list of @nixpkgs@ required for the prebuild script and/or analysis strategies
    repoDynamicNixDeps :: [Text],
    -- | A set of strategy test cases
    repoAnalyses :: [Analysis a]
  }

data Analysis a = Analysis
  { -- | Name of the strategy (use the strategy module name: Strategy.Cargo => "Cargo")
    analysisName :: String,
    -- | The strategy @discover@ function (Strategy.Cargo => Cargo.discover)
    analysisFinder :: Path Abs Dir -> TestC IO [a],
    analysisFunc :: a -> TestC IO (Graphing Dependency, GraphBreadth),
    analysisMkProject :: a -> DiscoveredProject (TestC IO),
    -- | The set of projects expected to be found + succeed under this strategy
    analysisProjects :: [TestProject]
  }

data TestProject = TestProject
  { testProjectPath :: Path Rel Dir,
    testProjectTargets :: FoundTargets
  } deriving (Show, Eq, Ord)

simpleTestProject :: Path Rel Dir -> TestProject
simpleTestProject base = TestProject base mempty

-- | Test harness: given a 'Repo', this creates test cases for each analysis
-- strategy
repo :: Repo a -> Spec
repo Repo {..} = beforeAll_ (traverse_ runScript repoPrebuildScript) $
  describe (toFilePath repoRoot) $ do
    absRepoRoot <- runIO $ PIO.makeAbsolute repoRoot
    traverse_ (single repoDynamicNixDeps absRepoRoot) repoAnalyses
  where
    runScript :: Path Rel File -> IO ()
    runScript scriptPath = do
      absScriptPath <- PIO.makeAbsolute scriptPath
      res <- runExecNix repoDynamicNixDeps . exec repoRoot $ scriptToCommand absScriptPath
      case res of
        Left err -> expectationFailure (show err)
        Right _ -> pure ()

-- | Execute a single anlysis, given a set of nix packages
single :: [Text] -> Path Abs Dir -> Analysis a -> Spec
single nixpkgs basedir Analysis {..} =
  it analysisName $ do
    discoveryResult <- runTestC nixpkgs $ analysisFinder basedir
    case discoveryResult of
      Left e -> failedDiag e
      Right projects -> do
        expectedProjects <- traverse (extractTestProject basedir . analysisMkProject) projects
        analysisProjects `shouldMatchList` expectedProjects
        for_ projects $ \project -> do
          analysisResult <- runTestC nixpkgs $ analysisFunc project
          case analysisResult of
            Left e -> failedDiag e
            Right _ -> () `shouldBe` ()

extractTestProject :: Path Abs Dir -> DiscoveredProject n -> IO TestProject
extractTestProject base disc = do
  projPath <- PIO.makeRelative base $ projectPath disc
  pure $ TestProject
    { testProjectPath = projPath,
      testProjectTargets = projectBuildTargets disc
    }

failedDiag :: FailureBundle -> Expectation
failedDiag = expectationFailure . show

scriptToCommand :: Path Abs File -> Command
scriptToCommand path =
  Command
    { cmdName = toText $ toFilePath path,
      cmdArgs = [],
      cmdAllowErr = Never
    }

type TestC m = ExecNixShellC (DiagnosticsC (ReadFSIOC (FinallyC (IgnoreLoggerC m ))))

runTestC :: [Text] -> TestC IO a -> IO (Either FailureBundle a)
runTestC nixpkgs =
  ignoreLogger
  . runFinally
  . runReadFSIO
  . runDiagnostics
  . runExecNix nixpkgs

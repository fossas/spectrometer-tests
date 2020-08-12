module Repo
  ( repo,
    Repo (..),
    Analysis (..),
  )
where

import qualified Control.Carrier.Diagnostics as Diag
import Control.Carrier.Exec.NixShell
import Control.Carrier.Finally
import Control.Carrier.Output.IO
import Control.Carrier.TaskPool
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Path
import qualified Path.IO as PIO
import Test.Hspec
import Types

data Repo = Repo
  { -- | Path to the root of the repo
    repoRoot :: Path Rel Dir,
    -- | An optional path to a build script required for the analysis strategies to succeed.
    -- The script is run with @repoRoot@ as the workdir.
    repoPrebuildScript :: Maybe (Path Rel File),
    -- | A list of @nixpkgs@ required for the prebuild script and/or analysis strategies
    repoDynamicNixDeps :: [Text],
    -- | A set of strategy test cases
    repoAnalyses :: [Analysis]
  }

data Analysis = Analysis
  { -- | Name of the strategy (use the strategy module name: Strategy.Cargo => "Cargo")
    analysisName :: String,
    -- | The strategy @discover@ function (Strategy.Cargo => Cargo.discover)
    analysisFunc :: Path Abs Dir -> TestC IO (),
    -- | The set of projects expected to be found + succeed under this strategy
    analysisProjects :: [Path Rel Dir]
  }

-- | Test harness: given a 'Repo', this creates test cases for each analysis
-- strategy
repo :: Repo -> Spec
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
single :: [Text] -> Path Abs Dir -> Analysis -> Spec
single nixpkgs basedir Analysis {..} =
  it analysisName $ do
    (closures, failures) <- runTestC nixpkgs $ analysisFunc basedir

    let closureDirs = map closureModuleDir closures
        absExpectedDirs = map (basedir </>) analysisProjects

    traverse_ (print . Diag.renderFailureBundle . projectFailureCause) failures
    closureDirs `shouldMatchList` absExpectedDirs

scriptToCommand :: Path Abs File -> Command
scriptToCommand path =
  Command
    { cmdName = T.pack $ toFilePath path,
      cmdArgs = [],
      cmdAllowErr = Never
    }

type TestC m = ExecNixShellC (ReadFSIOC (TaskPoolC (FinallyC (IgnoreLoggerC (OutputC ProjectFailure (OutputC ProjectClosure m))))))

-- | Given a set of nix packages, run the test carrier
runTestC :: [Text] -> TestC IO a -> IO ([ProjectClosure], [ProjectFailure])
runTestC nixpkgs =
  fmap (\(closures, (failures, ())) -> (closures, failures))
    . runOutput @ProjectClosure
    . runOutput @ProjectFailure
    . ignoreLogger
    . runFinally
    . withTaskPool 1 (const (pure ()))
    . runReadFSIO
    . runExecNix nixpkgs

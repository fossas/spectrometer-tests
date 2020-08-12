module Control.Carrier.Exec.NixShell
  ( runExecNix,
    ExecNixShellC,
  )
where

import Control.Monad.IO.Class
import Control.Carrier.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Effect.Exec

-- | An 'Exec' effect carrier that wraps each command invocation in a nix shell
newtype ExecNixShellC m a = ExecNixShellC {runExecNixShellC :: ReaderC [Text] (ExecIOC m) a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run the ExecNixShell carrier with a set of nix packages
runExecNix :: [Text] -> ExecNixShellC m a -> m a
runExecNix pkgs = runExecIO . runReader pkgs . runExecNixShellC

instance (MonadIO m, Algebra sig m) => Algebra (Exec :+: sig) (ExecNixShellC m) where
  alg hdl sig ctx = ExecNixShellC $ case sig of
    L (Exec dir cmd) -> do
      nixpkgs <- ask
      res <- exec dir (nixify nixpkgs cmd)
      pure (res <$ ctx)
    R other -> alg (runExecNixShellC . hdl) (R (R other)) ctx

nixify ::
  -- | packages to include
  [Text] ->
  Command ->
  Command
nixify packages cmd =
  Command
    { cmdName = "nix-shell",
      cmdArgs = ["-p"] <> packages <> ["--run"] <> [cmdName cmd <> " " <> T.intercalate " " (cmdArgs cmd)], -- TODO: escaping command args?
      cmdAllowErr = cmdAllowErr cmd
    }

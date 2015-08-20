{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#include "overlap.h"
__LANGUAGE_OVERLAPPING_INSTANCES__

module HERMIT.GHCI.Printer
        ( printForRepl
        ) where

import HERMIT.API.Types
import HERMIT.GHCI.Client

import HERMIT.API.Shell

import Control.Monad (unless)

class Repl a where
  printForRepl :: a -> IO ()

instance __OVERLAPPABLE__ Show a => Repl a where
  printForRepl = print

instance __OVERLAPPING__ (ShellSettings, Response a) => Repl (Shell a) where
  printForRepl sh = do
        r <- send sh
        printResponse r

instance ShellSettings => Repl KernelEffect where
  printForRepl (KernelEffect v) = printForRepl (Shell v :: Shell ())

instance ShellSettings => Repl (ShellEffect ()) where
  printForRepl (ShellEffect v) = printForRepl (Shell v :: Shell ())

instance ShellSettings => Repl ScriptEffect where
  printForRepl (ScriptEffect v) = printForRepl (Shell v :: Shell ())

instance ShellSettings => Repl QueryFun where
  printForRepl = printForRepl . queryFun

instance ShellSettings => Repl Crumb where
  printForRepl = printForRepl . run

instance (Guts a, ShellSettings) => Repl (Transform a LocalPath) where
  printForRepl = printForRepl . run

instance (Guts a, ShellSettings) => Repl (Transform a a) where
  printForRepl = printForRepl . run

{-
instance __OVERLAPPABLE__ Guts a => Repl (Transform a b) where
  printForRepl = printForRepl . run
-}

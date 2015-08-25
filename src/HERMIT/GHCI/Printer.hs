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

import Control.Monad (void)

class Repl a where
  printForRepl :: a -> IO ()

instance __OVERLAPPABLE__ Show a => Repl a where
  printForRepl = print

instance __OVERLAPPING__ (Response a) => Repl (Shell a) where
  printForRepl sh = void $ send sh

instance Repl KernelEffect where
  printForRepl (KernelEffect v) = printForRepl (Shell v :: Shell ())

instance Repl (ShellEffect ()) where
  printForRepl (ShellEffect v) = printForRepl (Shell v :: Shell ())

instance Repl ScriptEffect where
  printForRepl (ScriptEffect v) = printForRepl (Shell v :: Shell ())

instance Repl QueryFun where
  printForRepl = printForRepl . queryFun

instance Repl Crumb where
  printForRepl = printForRepl . run

instance (Guts a) => Repl (Transform a LocalPath) where
  printForRepl = printForRepl . run

instance (Guts a) => Repl (Transform a a) where
  printForRepl = printForRepl . run

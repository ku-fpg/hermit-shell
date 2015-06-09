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

import Control.Monad (unless)

class Repl a where
  printForRepl :: a -> IO ()

instance __OVERLAPPABLE__ Show a => Repl a where
  printForRepl = print

instance __OVERLAPPING__ Response a => Repl (Shell a) where
  printForRepl sh = do
        r <- send sh
        let txt = showResponse r
        unless (null txt) $ putStrLn txt

instance Repl KernelEffect where
  printForRepl (KernelEffect v) = printForRepl (Shell v :: Shell ())

instance Repl ShellEffect where
  printForRepl (ShellEffect v) = printForRepl (Shell v :: Shell ())

instance Repl ScriptEffect where
  printForRepl (ScriptEffect v) = printForRepl (Shell v :: Shell ())

instance Repl QueryFun where
  printForRepl (QueryFun v) = printForRepl (Shell v :: Shell ())

instance Repl (Transform a b) where
  printForRepl (Transform v) = printForRepl (Shell v :: Shell ())

instance Repl (BiTransform a b) where
  printForRepl (BiTransform v) = printForRepl (Shell v :: Shell ())


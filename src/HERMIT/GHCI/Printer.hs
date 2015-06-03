{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL <= 708 && __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module HERMIT.GHCI.Printer
        ( printForRepl
        ) where

import HERMIT.API.Types
import HERMIT.GHCI.Client

import Control.Monad (when)

class Repl a where
  printForRepl :: a -> IO ()

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPABLE #-}
#else
instance
#endif
    Show a => Repl a where
  printForRepl = print

#if __GLASGOW_HASKELL >= 710
instance {-# OVERLAPPING #-}
#else
instance
#endif
    Response a => Repl (Shell a) where
  printForRepl sh = do
        r <- send sh
        let txt = showResponse r
        when (not $ null $ txt) $ do
          putStrLn txt

instance Repl KernelEffect where
  printForRepl (KernelEffect v) = printForRepl (Shell v :: Shell ())

instance Repl ShellEffect where
  printForRepl (ShellEffect v) = printForRepl (Shell v :: Shell ())

instance Repl ScriptEffect where
  printForRepl (ScriptEffect v) = printForRepl (Shell v :: Shell ())

instance Repl QueryFun where
  printForRepl (QueryFun v) = printForRepl (Shell v :: Shell ())

instance Repl (BiTransform a b) where
  printForRepl (BiTransform v) = printForRepl (Shell v :: Shell ())


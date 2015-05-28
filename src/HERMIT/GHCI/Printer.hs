{-# LANGUAGE UndecidableInstances, FlexibleInstances, IncoherentInstances #-}
module HERMIT.GHCI.Printer
        ( printForRepl
        ) where

import HERMIT.API.Types
import HERMIT.GHCI.Client

-- There is some serious hackery here; it will be better in 7.10.

class Repl a where
  printForRepl :: a -> IO ()

instance {- OVERLAPPABLE -} Show a => Repl a where
  printForRepl = print
  
instance {- OVERLAPPING -} Show a => Repl (Shell a) where  
  printForRepl sh = do
        r <- send sh
        print r

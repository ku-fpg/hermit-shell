{-# LANGUAGE UndecidableInstances, FlexibleInstances, IncoherentInstances #-}
module HERMIT.GHCI.Printer
        ( printForRepl
        ) where

import HERMIT.API.Types
import HERMIT.GHCI.Client

import Control.Monad (when)

-- There is some serious hackery here; it will be better in 7.10.

class Repl a where
  printForRepl :: a -> IO ()

instance {- OVERLAPPABLE -} Show a => Repl a where
  printForRepl = print
  
instance {- OVERLAPPING -} Response a => Repl (Shell a) where  
  printForRepl sh = do
        r <- send sh
        let txt = showResponse r
        when (not $ null $ txt) $ do
          putStrLn txt

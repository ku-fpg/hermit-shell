{-# LANGUAGE OverloadedStrings #-}
module WWAssA where

import HERMIT.API

wwa :: Shell ()
wwa = do
  eval "www-result-split [| abs |] [| rep |] (w-result-AssA-to-AssC\
       \  { unfold 'abs }\
       \  { unfold 'rep }\
       \  { case-elim-merge-alts })" 


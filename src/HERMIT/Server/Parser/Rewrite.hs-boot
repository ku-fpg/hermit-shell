{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Rewrite () where

import HERMIT.Kure (LCore, LCoreTC, RewriteH)
import HERMIT.Server.Parser.Utils (External)

instance External (RewriteH LCore)
instance External (RewriteH LCoreTC)

{-# LANGUAGE OverloadedStrings #-}
import HERMIT.API

script :: Shell ()
script = do
  eval "load-as-rewrite \"WWA\" \"WW-Ass-A.hss\""
  apply flattenModule

  setPath $ bindingOf "fib"

  scope $ do
    eval "ww-split [| wrap |] [| unwrap |] (ww-AssA-to-AssC WWA)"
    setPath (bindingOf "work") ; query $ remember "origwork"

    -- work = unwrap (f (wrap work))

    sendCrumb defRhs ; apply $ etaExpand "n"

    -- work n = unwrap (f (wrap work)) n

    apply . anyCall $ unfoldWith "unwrap"

    -- work n = (f (wrap work) n, f (wrap work) (n+1))

    sendCrumb lamBody ; eval "case-split-inline 'n"

    -- work 0     = (f (wrap work) 0, f (wrap work) 1)
    -- work (n+1) = (f (wrap work) (n+1), f (wrap work) (n+2))

    scope $ do sendCrumb (caseAlt 0) ; apply . anyCall $ unfoldWith "f"
    scope $ do sendCrumb (caseAlt 1) ; sendCrumb altRhs ; sendCrumb appArg ; apply . anyCall $ unfoldWith "f"
    apply simplify

    -- work 0     = (0, 1)
    -- work (n+1) = (f (wrap work) (n+1), wrap work (n+1) + wrap work n)

    sendCrumb (caseAlt 1) ; sendCrumb altRhs
    scope $ do sendCrumb appArg ; apply $ anyCall (unfoldRemembered "origwork")

    -- work 0     = (0, 1)
    -- work (n+1) = (f (wrap work) (n+1), wrap (unwrap (f (wrap work))) (n+1) + wrap (unwrap (f (wrap work))) n)

    eval "any-bu (forward (ww-assumption-A [| wrap |] [| unwrap |] WWA ))"

    -- work 0     = (0, 1)
    -- work (n+1) = (f (wrap work) (n+1), f (wrap work) (n+1) + f (wrap work) n)

    scope $ do setPath $ arg 3 ; setPath $ arg 1 ; apply $ letIntro "x"
    scope $ do setPath $ arg 2 ; apply $ letIntro "y"
    apply $ innermost letFloat
    apply $ try (reorderLets ["x","y"])
    apply $ oneTD (fold "y")
    apply $ letTuple "xy"

    -- work 0     = (0, 1)
    -- work (n+1) = let (x,y) = (f (wrap work) n, f (wrap work) (n+1)) in (y,x+y)

    apply . oneTD $ fold "unwrap"

    -- work 0     = (0, 1)
    -- work (n+1) = let (x,y) = unwrap (f (wrap work)) n in (y,x+y)

    apply . oneTD $ foldRemembered "origwork"

    -- work 0     = (0, 1)
    -- work (n+1) = let (x,y) = work n in (y,x+y)

  scope $ do sendCrumb defRhs ; apply letElim

  apply . anyCall $ unfoldWith "wrap"


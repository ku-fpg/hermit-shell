module HanoiScript where
import HERMIT.API

script :: Shell ()
script = do
  apply flattenModule

  -- do the w/w split
  setPath $ bindingOf "hanoi"
  scope . apply $ wwSplitUnsafe "wrap" "unwrap"

  scope $ do
    setPath $ bindingOf "work"
    query $ remember "origwork"

    apply . anyCall $ unfoldWith "unwrap"

      -- establish the zero base case
    mapM_ sendCrumb [defRhs, lamBody, lamBody, lamBody, lamBody]
    eval "  case-split-inline 'n"
    scope $ do sendCrumb (caseAlt 0)
               apply . anyCall $ unfoldWith "f"
               apply simplify

    scope $ do
      -- establish the one base case
      mapM_ sendCrumb [caseAlt 1, altRhs]
      eval "case-split-inline 'a"

      scope $ do
        sendCrumb (caseAlt 0) ; apply . anyCall $ unfoldWith "f" ; apply simplify
        mapM_ apply
              [ anyCall (unfoldRemembered "origwork")
              , anyCall (forward (wwAssumptionAUnsafe "wrap" "unwrap"))
              , anyCall (unfoldWith "f")
              ]

        apply simplify
        apply . anyCall $ unfoldRule "[] ++"
        proofCmd assume
        --      any-call (unfold-rule "++ []")
      scope $ do
        sendCrumb $ caseAlt 1
        apply . anyCall $ unfoldWith "f"
        apply simplify

        apply . anyCall $ unfoldRemembered "origwork"
        apply . anyCall . forward $ wwAssumptionAUnsafe "wrap" "unwrap"
        apply . anyCall $ unfoldWith "f"
        apply $ innermost letSubst
        apply simplify

              -- recursion decrements by two, so must do this again
        apply . anyCall $ unfoldRemembered "origwork"
        apply . anyCall . forward $ wwAssumptionAUnsafe "wrap" "unwrap"

              -- time to let intro
              -- need a "occurance 'work" like consider
        scope $ do
          sendCrumb altRhs
          scope $ do
            setPath $ arg 5
            scope $ do
              setPath $ arg 1
              scope $ do
                setPath (arg 1) ; apply (letIntro "u")
              scope $ do
                setPath (arg 2) ; setPath (arg 2)
                apply $ letIntro "v"
            scope $ do
              setPath (arg 2)
              setPath (arg 2)
              setPath (arg 1)
              apply $ letIntro "w"
          apply $ innermost letFloat
          apply . try $ reorderLets ["u", "v", "w"]
          apply . anyCall $ fold "u"
          apply . anyCall $ fold "v"
          --        any-call (fold 'w)
          apply $ letTuple "uvw"
          apply . anyCall $ fold "unwrap"
          apply . anyCall $ foldRemembered "origwork"
  --innermost let-elim
  apply $ innermost letSubst


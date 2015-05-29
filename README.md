# HERMIT shell

Implements RESTful web service interface to HERMIT,
spawns GHCi in a separate process to communicate with it.

## Running

    cabal install
    hermit-shell SomeFile.hs

Once started, the only command is currently ':send'

    :send display
    :send rhs-of 'main

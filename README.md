# HERMIT shell

Implements RESTful web service interface to HERMIT,
spawns GHCi in a separate process to communicate with it.

## Running

    cabal install
    hermit-shell SomeFile.hs

Here is an example of how to use the `display` command and how set the current
path to the `main` function, once the shell is started:

    send display
    setPath (rhsOf "main")

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


### Tests


````
bash% cabal test --show-details=streaming
````

(Note that this runs the *installed* version, not the locally built version.)

To remaster any test, just remove the golden, and re-rerun.

### Runing Examples

    % cabal exec bash
    % cd examples/hanoi/
    % hermit-shell Hanoi.hs 
    [...]
    HERMIT> :l HanoiScript
    [...]
    HERMIT> script
    
    
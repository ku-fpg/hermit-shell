How to invoke. Make sure you are in the sandbox (cabal exec bash, in root)

````
% hermit-ghci Reverse.hs +Main 
[Boot screen]
HERMIT> send display
````

How to invoke the (orignal) version

````
% hermit Reverse.hs +Main Reverse.hss -safety=unsafe
````

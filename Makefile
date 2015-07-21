boot::
	cabal sandbox init 
	cabal sandbox add-source ../hermit
	cabal sandbox add-source ../kure
	cabal sandbox add-source ../remote-json
	cabal install --reorder-goals 
	cabal configure --enable-tests

hermit::
	cabal install  --only-dependencies      --force-reinstalls 

tests::
	cabal test --show-details=streaming

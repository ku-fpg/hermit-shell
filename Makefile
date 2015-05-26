boot::
	cabal sandbox init 
	cabal sandbox add-source ../hermit
	cabal sandbox add-source ../kure
	cabal install


hermit::
	cabal install  --only-dependencies      --force-reinstalls 
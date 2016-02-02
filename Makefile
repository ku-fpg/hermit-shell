boot::
	cabal sandbox init 
	cabal sandbox add-source \
		../hermit \
		../kure \
		../remote-json \
		../remote-json/remote-json-server \
		../remote-json/remote-json-client
	cabal install --reorder-goals --enable-tests --only-d
	cabal configure --enable-tests

hermit::
	cabal install  --only-dependencies      --force-reinstalls 
	cabal configure --enable-tests

build::
	cabal build

tests::
	cabal test --show-details=streaming

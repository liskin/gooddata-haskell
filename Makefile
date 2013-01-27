inplace: build
	cabal register --inplace

dist/setup-config: gooddata-haskell.cabal
	cabal configure

build: dist/setup-config
	cabal build

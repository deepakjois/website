.PHONY: build build-site clean-site preview-site deploy-site

build-site:
	cabal run build

build:
	cabal clean
	cabal configure
	cabal build

clean-site:
	cabal run clean

preview-site: build-site
	cabal run watch

deploy-site: build-site
	cabal run deploy


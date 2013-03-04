.PHONY: build build-site clean-site preview-site deploy-site

sitebin = dist/build/site/site

build-site:
	$(sitebin) build

build:
	cabal-dev --sandbox ../cabal-dev clean
	cabal-dev --sandbox ../cabal-dev configure
	cabal-dev --sandbox ../cabal-dev build

clean-site:
	$(sitebin) clean

preview-site: build-site
	$(sitebin) preview

deploy-site: build-site
	$(sitebin) deploy


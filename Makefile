.PHONY: build build-site clean-site preview-site deploy-site

sitebin = .cabal-sandbox/bin/site

build-site:
	$(sitebin) build

build:
	cabal clean
	cabal configure
	cabal install

clean-site:
	$(sitebin) clean

preview-site: build-site
	$(sitebin) preview

deploy-site: build-site
	$(sitebin) deploy


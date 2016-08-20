.PHONY: build build-css build-site clean-site preview-site deploy-site

build-site:
	sass sass/style.scss:static/css/style.css
	site build

build:
	stack clean
	stack build
	stack install

clean-site:
	site clean

preview-site: build-site
	devd -ol _site

deploy-site: build-site
	./upload _site


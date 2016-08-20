.PHONY: build build-css copy-css build-site clean-site preview-site deploy-site

build-site: build-css
	site build

build-css:
	sass sass/style.scss:static/css/style.css

copy-css: build-css
	cp static/css/style.css _site/css/

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


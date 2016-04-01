.PHONY: build build-site clean-site preview-site deploy-site

build-site:
	site build

build:
	stack clean
	stack build
	stack install

clean-site:
	site clean

preview-site: build-site
	devd -ol .

deploy-site: build-site
	./upload _site


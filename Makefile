all: build test

submodules:
	git submodule init
	git submodule update

build: submodules
	appmock/make.py -s appmock -r .
	globalregistry/make.py -s globalregistry -r .
	oneclient/make.py -s oneclient -r . release
	oneprovider/make.py -s oneprovider -r .

test:
	./test_run.py

clean_all:
	appmock/make.py -s appmock -r . clean
	globalregistry/make.py -s globalregistry -r . clean
	oneclient/make.py -s oneclient -r . clean
	oneprovider/make.py -s oneprovider -r . clean

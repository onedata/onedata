all: build test

submodules:
	git submodule init
	git submodule update

build: submodules
	cd appmock && ./make.py -r .. && cd ..
	cd globalregistry && ./make.py -r .. && cd ..
	cd oneclient && ./make.py -r .. && cd ..
	cd oneprovider && ./make.py -r .. && cd ..

test:
	py.test tests -s

clean_all:
	cd appmock && ./make.py -r .. clean && cd ..
	cd globalregistry && ./make.py -r .. clean && cd ..
	cd oneclient && ./make.py -r .. clean && cd ..
	cd oneprovider && ./make.py -r .. clean && cd ..

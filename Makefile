all: build test

build:
	cd appmock && ./make.py && cd ..
	cd globalregistry && ./make.py && cd ..
	cd oneclient && ./make.py && cd ..
	cd oneprovider && ./make.py && cd ..

test:
	py.test tests

clean_all:
	cd appmock && ./make.py clean && cd ..
	cd globalregistry && ./make.py clean && cd ..
	cd oneclient && ./make.py clean && cd ..
	cd oneprovider && ./make.py clean && cd ..

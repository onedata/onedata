all: build test package_oneprovider

submodules:
	git submodule init
	git submodule update

build: submodules
	appmock/make.py -s appmock -r .
	globalregistry/make.py -s globalregistry -r .
	oneclient/make.py -s oneclient -r . release
	oneprovider/make.py -s oneprovider -r .
	op_ccm/make.py -s op_ccm -r .

test:
	./test_run.py

clean_all:
	appmock/make.py -s appmock -r . clean
	globalregistry/make.py -s globalregistry -r . clean
	oneclient/make.py -s oneclient -r . clean
	oneprovider/make.py -s oneprovider -r . clean
	onepanel/make.py -s onepanel -r . clean

package_oneprovider: package_op_onepanel package_op_worker package_op_ccm
	bamboos/docker/make.py -s oneprovider_meta -r . -c 'dpkg-deb -b oneprovider'

package_op_onepanel:
	onepanel/make.py -s onepanel -r . clean
	onepanel/make.py -s onepanel -r . package
	bamboos/docker/package.py -i onedata/builder:v9 -s onepanel -r . -c \
	'cd package ; sbuild -d sid --add-depends=git --add-depends=erlang \
	--add-depends=erlang-src --add-depends=libbotan1.10-dev \
	--add-depends=pkg-config --add-depends=ssh op-onepanel_*.dsc'
	cp onepanel/package/*.deb package/

package_op_worker:
	oneprovider/make.py -s oneprovider -r . clean
	oneprovider/make.py -s oneprovider -r . package
	bamboos/docker/package.py -i onedata/builder:v9 -s oneprovider -r . -c \
	'cd package ; sbuild -d sid --add-depends=git --add-depends=erlang \
	--add-depends=erlang-src --add-depends=libbotan1.10-dev \
	--add-depends=pkg-config --add-depends=ssh  --add-depends=libprotobuf-dev \
	--add-depends=libprotoc-dev --add-depends=protobuf-compiler \
	--add-depends=cmake --add-depends=libssl-dev \
	--add-depends=libglobus-common-dev --add-depends=libglobus-gsi-callback-dev \
	--add-depends=libboost-filesystem1.58-dev \
	--add-depends=libboost-program-options1.58-dev \
    --add-depends=libboost-python1.58-dev --add-depends=libboost-random1.58-dev \
    --add-depends=libboost-system1.58-dev --add-depends=libboost-thread1.58-dev \
	oneprovider-node_*.dsc'
	cp oneprovider/package/*.deb package/

package_op_ccm:
	op_ccm/make.py -s op_ccm -r . clean
	op_ccm/make.py -s op_ccm -r . package
	bamboos/docker/package.py -i onedata/builder:v9 -s op_ccm -r . -c 'cd package ; sbuild -d sid --add-depends=git --add-depends=erlang --add-depends=erlang-src --add-depends=libbotan1.10-dev --add-depends=pkg-config --add-depends=ssh op-ccm_*.dsc'
	cp op_ccm/package/*.deb package/

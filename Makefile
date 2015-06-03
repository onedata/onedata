all: build package_oneprovider test

MAKE_APPMOCK := appmock/make.py -s appmock -r .
MAKE_ONEPANEL := onepanel/make.py -s onepanel -r .
MAKE_GLOBALREGISTRY := globalregistry/make.py -s onepanel -r .
MAKE_ONECLIENT := oneclient/make.py -s oneclient -r .
MAKE_OP_WORKER := op_worker/make.py -s op_worker -r .
MAKE_OP_CCM := op_ccm/make.py -s op_ccm -r .

make = $(1)/make.py -s $(1) -r .
clean = $(call make, $(1)) clean
make_rpm = $(call make, $(1)) --privileged --group mock -i onedata/rpm_builder package
mv_rpm = mv $(1)/package/packages/*.src.rpm* rpm/SRCS && mv $(1)/package/packages/*.x86_64.rpm* rpm/x86_64

submodules:
	git submodule init
	git submodule update

build: build_appmock build_globalregistry build_oneclient build_op_worker build_op_ccm

build_appmock: submodules
	$(call make, appmock)

build_globalregistry: submodules
	$(call make, globalregistry)

build_oneclient: submodules
	$(call make, oneclient) release

build_op_worker: submodules
	$(call make, op_worker)

build_op_ccm: submodules
	$(call make, op_ccm)

test:
	./test_run.py

clean_all: clean_appmock clean_globalregistry clean_oneclient \
           clean_op_worker clean_onepanel clean_op_ccm

clean_appmock:
	$(call clean, appmock)

clean_onepanel:
	$(call clean, onepanel)

clean_globalregistry:
	$(call clean, globalregistry)

clean_op_worker:
	$(call clean, op_worker)

clean_oneclient:
	$(call clean, oneclient)

clean_op_ccm:
	$(call clean, op_ccm)

rpm_oneprovider: rpm_op_onepanel rpm_op_worker rpm_op_ccm
	rm -Rf oneprovider_meta/package/packages

	bamboos/docker/make.py -i onedata/rpm_builder --privileged --group mock -c \
	        mock --buildsrpm --spec oneprovider_meta/oneprovider.spec \
	        --sources oneprovider_meta \
	        --resultdir oneprovider_meta/package/packages

	bamboos/docker/make.py -i onedata/rpm_builder --privileged --group mock -c \
	        mock --rebuild oneprovider_meta/package/packages/*.src.rpm \
	        --resultdir oneprovider_meta/package/packages

	$(call mv_rpm, oneprovider_meta)

rpm_op_onepanel: clean_onepanel rpmdirs
	$(call make_rpm, onepanel) -e REL_TYPE=oneprovider
	$(call mv_rpm, onepanel)

rpm_op_worker: clean_op_worker rpmdirs
	$(call make_rpm, oneprovider)
	$(call mv_rpm, oneprovider)

rpm_op_ccm: clean_op_ccm rpmdirs
	$(call make_rpm, op_ccm)
	$(call mv_rpm, op_ccm)

rpmdirs:
	mkdir -p rpm/SRCS rpm/x86_64

package_oneprovider: package_op_onepanel package_op_worker package_op_ccm
	bamboos/docker/make.py -s oneprovider_meta -r . -c 'dpkg-deb -b oneprovider'

package_op_onepanel: clean_onepanel
	$(call make, onepanel) package
	bamboos/docker/package.py -i onedata/builder:v9 -s onepanel -r . -c \
	'cd package ; sbuild -d sid --add-depends=git --add-depends=erlang \
	--add-depends=erlang-src --add-depends=libbotan1.10-dev \
	--add-depends=pkg-config --add-depends=ssh op-onepanel_*.dsc'
	cp onepanel/package/*.deb package/

package_op_worker: clean_op_worker
	$(call make, op_worker) package
	bamboos/docker/package.py -i onedata/builder:v9 -s op_worker -r . -c \
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
	op-worker_*.dsc'
	cp op_worker/package/*.deb package/

package_op_ccm: clean_op_ccm
	$(call make, op_ccm) package
	bamboos/docker/package.py -i onedata/builder:v9 -s op_ccm -r . -c 'cd package ; sbuild -d sid --add-depends=git --add-depends=erlang --add-depends=erlang-src --add-depends=libbotan1.10-dev --add-depends=pkg-config --add-depends=ssh op-ccm_*.dsc'
	cp op_ccm/package/*.deb package/

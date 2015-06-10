PKG_VERSION	    ?= $(shell git describe --tags --always | tr - .)

all: build deb_oneprovider rpm_oneprovider test

##
## Macros
##

MAKE_APPMOCK := appmock/make.py -s appmock -r .
MAKE_ONEPANEL := onepanel/make.py -s onepanel -r .
MAKE_GLOBALREGISTRY := globalregistry/make.py -s onepanel -r .
MAKE_ONECLIENT := oneclient/make.py -s oneclient -r .
MAKE_OP_WORKER := op_worker/make.py -s op_worker -r .
MAKE_OP_CCM := op_ccm/make.py -s op_ccm -r .

make = $(1)/make.py -s $(1) -r .
clean = $(call make, $(1)) clean
make_rpm = $(call make, $(1)) --privileged --group mock -i onedata/rpm_builder package
mv_rpm = mv $(1)/package/packages/*.src.rpm* package/rpm/SRPMS && mv $(1)/package/packages/*.x86_64.rpm* package/rpm/x86_64
make_deb = $(call make, $(1)) --privileged --group sbuild -i onedata/deb_builder package
mv_deb = mv $(1)/package/packages/*.orig.tar.gz package/deb/source && \
	mv $(1)/package/packages/*.dsc package/deb/source && \
	mv $(1)/package/packages/*.diff.gz package/deb/source && \
	mv $(1)/package/packages/*_amd64.changes package/deb/source && \
	mv $(1)/package/packages/*_amd64.deb package/deb/binary-amd64

##
## Submodules
##

submodules:
	git submodule init
	git submodule update

##
## Build
##

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

##
## Test
##

test:
	./test_run.py --test-dir tests/acceptance

test_packaging:
	./test_run.py --test-dir tests/packaging

##
## Clean
##

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

##
## RPM packaging
##

rpm_oneprovider: rpm_op_onepanel rpm_op_worker rpm_op_ccm
	rm -Rf oneprovider_meta/package/packages
	sed -i 's/Version:.*/Version:\t$(PKG_VERSION)/g' oneprovider_meta/oneprovider.spec

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
	$(call make_rpm, op_worker)
	$(call mv_rpm, op_worker)

rpm_op_ccm: clean_op_ccm rpmdirs
	$(call make_rpm, op_ccm)
	$(call mv_rpm, op_ccm)

rpmdirs:
	mkdir -p package/rpm/SRPMS package/rpm/x86_64

##
## DEB packaging
##

deb_oneprovider: deb_op_onepanel deb_op_worker deb_op_ccm
	sed -i 's/Version:.*/Version: $(PKG_VERSION)/g' oneprovider_meta/oneprovider/DEBIAN/control
	bamboos/docker/make.py -s oneprovider_meta -r . -c 'dpkg-deb -b oneprovider'
	mv oneprovider_meta/*.deb package/deb/binary-amd64

deb_op_onepanel: clean_onepanel debdirs
	$(call make_deb, onepanel) -e REL_TYPE=oneprovider
	$(call mv_deb, onepanel)

deb_op_worker: clean_op_worker debdirs
	$(call make_deb, op_worker)
	$(call mv_deb, op_worker)

deb_op_ccm: clean_op_ccm debdirs
	$(call make_deb, op_ccm)
	$(call mv_deb, op_ccm)

debdirs:
	mkdir -p package/deb/source package/deb/binary-amd64
# distro for package building (oneof: vivid, fedora-21-x86_64)
DISTRIBUTION            ?= none
export DISTRIBUTION

ONEPROVIDER_VERSION	    ?= $(shell git describe --tags --always | tr - .)
ONEPROVIDER_BUILD	    ?= 1
CLUSTER_MANAGER_VERSION	?= $(shell git -C cluster_manager describe --tags --always | tr - .)
OP_WORKER_VERSION		?= $(shell git -C op_worker describe --tags --always | tr - .)
OP_PANEL_VERSION		?= $(shell git -C onepanel describe --tags --always | tr - .)

.PHONY: package.tar.gz

all: build deb_oneprovider rpm_oneprovider test

##
## Macros
##

MAKE_APPMOCK := appmock/make.py -s appmock -r .
MAKE_ONEPANEL := onepanel/make.py -s onepanel -r .
MAKE_GLOBALREGISTRY := globalregistry/make.py -s globalregistry -r .
MAKE_ONECLIENT := oneclient/make.py -s oneclient -r .
MAKE_OP_WORKER := op_worker/make.py -s op_worker -r .
MAKE_CLUSTER_MANAGER := cluster_manager/make.py -s cluster_manager -r .

make = $(1)/make.py -s $(1) -r .
clean = $(call make, $(1)) clean
make_rpm = $(call make, $(1)) -e DISTRIBUTION=$(DISTRIBUTION) --privileged --group mock -i onedata/rpm_builder $(2)
mv_rpm = mv $(1)/package/packages/*.src.rpm package/$(DISTRIBUTION)/SRPMS && \
	mv $(1)/package/packages/*.x86_64.rpm package/$(DISTRIBUTION)/x86_64
make_deb = $(call make, $(1)) -e DISTRIBUTION=$(DISTRIBUTION) --privileged --group sbuild -i onedata/deb_builder $(2)
mv_deb = mv $(1)/package/packages/*.orig.tar.gz package/$(DISTRIBUTION)/source && \
	mv $(1)/package/packages/*.dsc package/$(DISTRIBUTION)/source && \
	mv $(1)/package/packages/*.diff.gz package/$(DISTRIBUTION)/source || \
	mv $(1)/package/packages/*.debian.tar.xz package/$(DISTRIBUTION)/source && \
	mv $(1)/package/packages/*_amd64.changes package/$(DISTRIBUTION)/source && \
	mv $(1)/package/packages/*_amd64.deb package/$(DISTRIBUTION)/binary-amd64

##
## Submodules
##

branch = $(shell git rev-parse --abbrev-ref HEAD)
submodules:
	./onedata_submodules.sh init
ifeq ($(branch),develop)
	./onedata_submodules.sh update --remote
else
	./onedata_submodules.sh update
endif

##
## Build
##

build: build_bamboos build_appmock build_globalregistry build_oneclient build_op_worker build_cluster_manager

build_bamboos: submodules
	$(call make, bamboos)

build_appmock: submodules
	$(call make, appmock)

build_globalregistry: submodules
	$(call make, globalregistry)

build_oneclient: submodules
	$(call make, oneclient) release

build_op_worker: submodules
	$(call make, op_worker)

build_cluster_manager: submodules
	$(call make, cluster_manager)

##
## Test
##

test:
	./test_run.py --test-dir tests/acceptance

test_packaging: build_globalregistry
	./test_run.py --test-dir tests/packaging -s

test_cucumber:
	./test_run.py --test-dir tests/cucumber

##
## Clean
##

clean_all: clean_appmock clean_globalregistry clean_oneclient \
           clean_op_worker clean_onepanel clean_cluster_manager clean_packages

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

clean_cluster_manager:
	$(call clean, cluster_manager)

clean_packages:
	rm -rf oneprovider_meta/oneprovider.spec \
		oneprovider_meta/oneprovider/DEBIAN/control \
		oneprovider_meta/package package

##
## RPM packaging
##

rpm_oneprovider: rpm_op_panel rpm_op_worker rpm_cluster_manager
	cp -f oneprovider_meta/oneprovider.spec.template oneprovider_meta/oneprovider.spec
	sed -i 's/{{oneprovider_version}}/$(ONEPROVIDER_VERSION)/g' oneprovider_meta/oneprovider.spec
	sed -i 's/{{oneprovider_build}}/$(ONEPROVIDER_BUILD)/g' oneprovider_meta/oneprovider.spec
	sed -i 's/{{cluster_manager_version}}/$(CLUSTER_MANAGER_VERSION)/g' oneprovider_meta/oneprovider.spec
	sed -i 's/{{op_worker_version}}/$(OP_WORKER_VERSION)/g' oneprovider_meta/oneprovider.spec
	sed -i 's/{{op_panel_version}}/$(OP_PANEL_VERSION)/g' oneprovider_meta/oneprovider.spec

	bamboos/docker/make.py -i onedata/rpm_builder --privileged --group mock -c \
	        mock --buildsrpm --spec oneprovider_meta/oneprovider.spec \
	        --sources oneprovider_meta --root $(DISTRIBUTION) \
	        --resultdir oneprovider_meta/package/packages

	bamboos/docker/make.py -i onedata/rpm_builder --privileged --group mock -c \
	        mock --rebuild oneprovider_meta/package/packages/*.src.rpm \
	        --root $(DISTRIBUTION) --resultdir oneprovider_meta/package/packages

	$(call mv_rpm, oneprovider_meta)

rpm_op_panel: clean_onepanel rpmdirs
	$(call make_rpm, onepanel, package) -e REL_TYPE=oneprovider \
	    -e COUCHBASE_SERVER_SERVICE="service couchbase-server-community"
	$(call mv_rpm, onepanel)

rpm_op_worker: clean_op_worker rpmdirs
	$(call make_rpm, op_worker, package)
	$(call mv_rpm, op_worker)

rpm_cluster_manager: clean_cluster_manager rpmdirs
	$(call make_rpm, cluster_manager, package)
	$(call mv_rpm, cluster_manager)

rpm_oneclient: clean_oneclient rpmdirs
	$(call make_rpm, oneclient, rpm)
	$(call mv_rpm, oneclient)

rpmdirs:
	mkdir -p package/$(DISTRIBUTION)/SRPMS package/$(DISTRIBUTION)/x86_64

##
## DEB packaging
##

deb_oneprovider: deb_op_panel deb_op_worker deb_cluster_manager
	cp -f oneprovider_meta/oneprovider/DEBIAN/control.template oneprovider_meta/oneprovider/DEBIAN/control
	sed -i 's/{{oneprovider_version}}/$(ONEPROVIDER_VERSION)/g' oneprovider_meta/oneprovider/DEBIAN/control
	sed -i 's/{{oneprovider_build}}/$(ONEPROVIDER_BUILD)/g' oneprovider_meta/oneprovider/DEBIAN/control
	sed -i 's/{{cluster_manager_version}}/$(CLUSTER_MANAGER_VERSION)/g' oneprovider_meta/oneprovider/DEBIAN/control
	sed -i 's/{{op_worker_version}}/$(OP_WORKER_VERSION)/g' oneprovider_meta/oneprovider/DEBIAN/control
	sed -i 's/{{op_panel_version}}/$(OP_PANEL_VERSION)/g' oneprovider_meta/oneprovider/DEBIAN/control

	bamboos/docker/make.py -s oneprovider_meta -r . -c 'dpkg-deb -b oneprovider'
	mv oneprovider_meta/oneprovider.deb package/$(DISTRIBUTION)/binary-amd64/oneprovider_$(ONEPROVIDER_VERSION)-$(ONEPROVIDER_BUILD)_amd64.deb

deb_op_panel: clean_onepanel debdirs
	$(call make_deb, onepanel, package) -e REL_TYPE=oneprovider \
	    -e COUCHBASE_SERVER_SERVICE="service couchbase-server"
	$(call mv_deb, onepanel)

deb_op_worker: clean_op_worker debdirs
	$(call make_deb, op_worker, package)
	$(call mv_deb, op_worker)

deb_cluster_manager: clean_cluster_manager debdirs
	$(call make_deb, cluster_manager, package)
	$(call mv_deb, cluster_manager)

deb_oneclient: clean_oneclient debdirs
	$(call make_deb, oneclient, deb)
	$(call mv_deb, oneclient)

debdirs:
	mkdir -p package/$(DISTRIBUTION)/source package/$(DISTRIBUTION)/binary-amd64

##
## Package artifact
##

package.tar.gz:
	tar -chzf package.tar.gz package

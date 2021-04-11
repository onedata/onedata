# distro for package building (oneof: xenial, bionic, focal, centos-7-x86_64)
DISTRIBUTION            ?= none
RELEASE                 ?= $(shell cat ./RELEASE)
DOCKER_RELEASE          ?= development
DOCKER_REG_NAME         ?= "docker.onedata.org"
DOCKER_REG_USER         ?= ""
DOCKER_REG_PASSWORD     ?= ""
PROD_RELEASE_BASE_IMAGE ?= "onedata/oneprovider-common:2002-2"
DEV_RELEASE_BASE_IMAGE  ?= "onedata/oneprovider-dev-common:2002-2"
HTTP_PROXY              ?= "http://proxy.devel.onedata.org:3128"
CONDA_TOKEN             ?= ""
CONDA_BUILD_OPTIONS     ?= ""

ifeq ($(strip $(ONEPROVIDER_VERSION)),)
ONEPROVIDER_VERSION     := $(shell git describe --tags --always --abbrev=7)
endif
ifeq ($(strip $(COUCHBASE_VERSION)),)
COUCHBASE_VERSION       := 4.5.1-2844
endif
ifeq ($(strip $(CLUSTER_MANAGER_VERSION)),)
CLUSTER_MANAGER_VERSION := $(shell git -C cluster_manager describe --tags --always --abbrev=7)
endif
ifeq ($(strip $(OP_WORKER_VERSION)),)
OP_WORKER_VERSION       := $(shell git -C op_worker describe --tags --always --abbrev=7)
endif
ifeq ($(strip $(OP_PANEL_VERSION)),)
OP_PANEL_VERSION        := $(shell git -C onepanel describe --tags --always --abbrev=7)
endif
ifeq ($(strip $(ONECLIENT_VERSION)),)
ONECLIENT_VERSION       := $(shell git -C oneclient describe --tags --always --abbrev=7)
endif
ifeq ($(strip $(FSONEDATAFS_VERSION)),)
FSONEDATAFS_VERSION       := $(shell git -C fs-onedatafs describe --tags --always --abbrev=7)
endif
ifeq ($(strip $(ONEDATAFS_JUPYTER_VERSION)),)
ONEDATAFS_JUPYTER_VERSION       := $(shell git -C onedatafs-jupyter describe --tags --always --abbrev=7)
endif
ifeq ($(strip $(ONEDATAFS_JUPYTER_VERSION)),)
ONEDATAFS_JUPYTER_VERSION       := $(shell git -C onedatafs-jupyter describe --tags --always)
endif



ONEPROVIDER_VERSION           := $(shell echo ${ONEPROVIDER_VERSION} | tr - .)
CLUSTER_MANAGER_VERSION       := $(shell echo ${CLUSTER_MANAGER_VERSION} | tr - .)
OP_WORKER_VERSION             := $(shell echo ${OP_WORKER_VERSION} | tr - .)
OP_PANEL_VERSION              := $(shell echo ${OP_PANEL_VERSION} | tr - .)
ONECLIENT_VERSION             := $(shell echo ${ONECLIENT_VERSION} | tr - .)
FSONEDATAFS_VERSION           := $(shell echo ${FSONEDATAFS_VERSION} | tr - .)
ONEDATAFS_JUPYTER_VERSION     := $(shell echo ${ONEDATAFS_JUPYTER_VERSION} | tr - .)

ONEPROVIDER_BUILD       ?= 1
ONECLIENT_FPMPACKAGE_TMP ?= package_fpm

ifdef IGNORE_XFAIL
TEST_RUN := ./test_run.py --ignore-xfail
else
TEST_RUN := ./test_run.py
endif

ifdef ENV_FILE
TEST_RUN := $(TEST_RUN) --env-file $(ENV_FILE)
endif


GIT_URL := $(shell git config --get remote.origin.url | sed -e 's/\(\/[^/]*\)$$//g')
GIT_URL := $(shell if [ "${GIT_URL}" = "file:/" ]; then echo 'ssh://git@git.onedata.org:7999/vfs'; else echo ${GIT_URL}; fi)
ONEDATA_GIT_URL := $(shell if [ "${ONEDATA_GIT_URL}" = "" ]; then echo ${GIT_URL}; else echo ${ONEDATA_GIT_URL}; fi)
export ONEDATA_GIT_URL

.PHONY: docker docker-dev package.tar.gz

all: build

##
## Macros
##

NO_CACHE :=  $(shell if [ "${NO_CACHE}" != "" ]; then echo "--no-cache"; fi)

make = $(1)/make.py -s $(1) -r . $(NO_CACHE)
clean = $(call make, $(1)) clean
make_rpm = $(call make, $(1)) -e DISTRIBUTION=$(DISTRIBUTION) -e RELEASE=$(RELEASE) --privileged --group mock -i onedata/rpm_builder:$(DISTRIBUTION)-$(RELEASE) $(2)
mv_rpm = mv $(1)/package/packages/*.src.rpm package/$(DISTRIBUTION)/SRPMS && \
	mv $(1)/package/packages/*.x86_64.rpm package/$(DISTRIBUTION)/x86_64
mv_noarch_rpm = mv $(1)/package/packages/*.src.rpm package/$(DISTRIBUTION)/SRPMS && \
	mv $(1)/package/packages/*.noarch.rpm package/$(DISTRIBUTION)/x86_64
make_deb = $(call make, $(1)) -e DISTRIBUTION=$(DISTRIBUTION) --privileged --group sbuild -i onedata/deb_builder:$(DISTRIBUTION)-$(RELEASE) $(2)
mv_deb = mv $(1)/package/packages/*_amd64.deb package/$(DISTRIBUTION)/binary-amd64 && \
	mv $(1)/package/packages/*.tar.gz package/$(DISTRIBUTION)/source | true && \
	mv $(1)/package/packages/*.dsc package/$(DISTRIBUTION)/source | true && \
	mv $(1)/package/packages/*.diff.gz package/$(DISTRIBUTION)/source | true && \
	mv $(1)/package/packages/*.debian.tar.xz package/$(DISTRIBUTION)/source | true && \
	mv $(1)/package/packages/*.changes package/$(DISTRIBUTION)/source | true
mv_noarch_deb = mv $(1)/package/packages/*_all.deb package/$(DISTRIBUTION)/binary-amd64 && \
	mv $(1)/package/packages/*.tar.gz package/$(DISTRIBUTION)/source | true && \
	mv $(1)/package/packages/*.dsc package/$(DISTRIBUTION)/source | true && \
	mv $(1)/package/packages/*.diff.gz package/$(DISTRIBUTION)/source | true && \
	mv $(1)/package/packages/*.debian.tar.xz package/$(DISTRIBUTION)/source | true && \
	mv $(1)/package/packages/*.changes package/$(DISTRIBUTION)/source | true
unpack = tar xzf $(1).tar.gz
make_conda = $(call make, $(1)) -e CONDA_TOKEN=$(CONDA_TOKEN) -i onedata/conda:v1 $(2)

get_release:
	@echo $(RELEASE)

print_package_versions:
	@echo "oneprovider:\t\t" $(ONEPROVIDER_VERSION)
	@echo "cluster-manager:\t" $(CLUSTER_MANAGER_VERSION)
	@echo "op-worker:\t\t" $(OP_WORKER_VERSION)
	@echo "op-panel:\t\t" $(OP_PANEL_VERSION)
	@echo "oneclient:\t\t" $(ONECLIENT_VERSION)
	@echo "fs-onedatafs:\t\t" $(FSONEDATAFS_VERSION)
	@echo "onedatafs-jupyter:\t" $(ONEDATAFS_JUPYTER_VERSION)

##
## Submodules
##

branch = $(shell git rev-parse --abbrev-ref HEAD)
submodules:
	git submodule sync --recursive ${submodule}
	git submodule update --init --recursive ${submodule}

##
## Build
##

build: build_appmock build_oz_worker build_oneclient build_op_worker \
    build_cluster_manager build_cluster_worker build_onepanel

build_appmock: submodules
	$(call make, appmock)

build_oz_worker: submodules
	$(call make, oz_worker)

build_oneclient: submodules
	$(call make, oneclient) deb-info

build_op_worker: submodules
	$(call make, op_worker)

build_cluster_manager: submodules
	$(call make, cluster_manager)

build_cluster_worker: submodules
	$(call make, cluster_worker)

build_onepanel: submodules
	$(call make, onepanel)

##
## Artifacts
##

artifact: artifact_appmock artifact_oneclient artifact_op_worker \
    artifact_oz_worker artifact_cluster_manager artifact_cluster_worker \
    artifact_onepanel

artifact_appmock:
	$(call unpack, appmock)

artifact_oneclient:
	$(call unpack, oneclient)

artifact_op_worker:
	$(call unpack, op_worker)

artifact_oz_worker:
	$(call unpack, oz_worker)

artifact_cluster_manager:
	$(call unpack, cluster_manager)

artifact_cluster_worker:
	$(call unpack, cluster_worker)

artifact_onepanel:
	$(call unpack, onepanel)

##
## Test
##

BROWSER             ?= Chrome
RECORDING_OPTION    ?= failed


test_env_up:
	${TEST_RUN} --test-type env_up -vvv --test-dir tests/env_up

test_provider_packaging test_packaging:
	${TEST_RUN} --test-type packaging -k "oneprovider" -vvv --test-dir tests/packaging -s

test_oneclient_base_packaging:
	${TEST_RUN} --test-type packaging -k "oneclient_base" -vvv --test-dir tests/packaging -s

test_oneclient_packaging:
	${TEST_RUN} --test-type packaging -k "oneclient and not oneclient_base" -vvv --test-dir tests/packaging -s

test_fsonedatafs_packaging:
	${TEST_RUN} --test-type packaging -k "fsonedatafs" -vvv --test-dir tests/packaging -s

test:
	${TEST_RUN} --test-type acceptance -vvv --test-dir tests/acceptance/scenarios/${SUITE}.py

test_performance:
	${TEST_RUN} -s --test-type performance -vvv --test-dir tests/performance

test_performance_dd:
	${TEST_RUN} -s --test-type performance -vvv --test-dir tests/performance -k test_dd

test_performance_sysbench:
	${TEST_RUN} -s --test-type performance -vvv --test-dir tests/performance -k sysbench

test_performance_files_creation:
	${TEST_RUN} -s --test-type performance -vvv --test-dir tests/performance -k test_files_creation

test_performance_concurrent_files_creation:
	${TEST_RUN} -s --test-type performance -vvv --test-dir tests/performance -k concurrent_files_creation

test_performance_transfer_onf:
	${TEST_RUN} -s --test-type performance -vvv --test-dir tests/performance -k transfer_onf

test_gui_env_up:
	${TEST_RUN} --test-type gui -vvv --test-dir tests/gui/scenarios/${SUITE}.py -i onedata/acceptance_gui:v3 --driver=${BROWSER} --basetemp=./tests/gui/tmp_files --showlocals --xvfb --xvfb-recording=${RECORDING_OPTION}

test_acceptance_mixed:
	${TEST_RUN} --test-type mixed -vvv --test-dir tests/mixed/scenarios/${SUITE}.py -i onedata/acceptance_mixed:v3 --driver=Chrome --xvfb --xvfb-recording=failed

test_profiling:
	${TEST_RUN} --test-type acceptance -vvv --test-dir tests/acceptance/profiling

##
## Clean
##

clean_all: clean_appmock clean_oz_worker clean_oneclient \
           clean_op_worker clean_onepanel clean_cluster_manager \
           clean_cluster_worker clean_packages

clean_appmock:
	$(call clean, appmock)

clean_onepanel:
	$(call clean, onepanel)

clean_oz_worker:
	$(call clean, oz_worker)

clean_op_worker:
	$(call clean, op_worker)

clean_oneclient:
	$(call clean, oneclient)

clean_fsonedatafs:
	$(call clean, fs-onedatafs)

clean_onedatafs_jupyter:
	$(call clean, onedatafs-jupyter)

clean_cluster_manager:
	$(call clean, cluster_manager)

clean_cluster_worker:
	$(call clean, cluster_worker)

clean_packages:
	rm -rf oneprovider_meta/oneprovider.spec \
		oneprovider_meta/oneprovider/DEBIAN/control \
		oneprovider_meta/package package oneclient_package_tmp

##
## RPM packaging
##

rpm: rpm_oneprovider rpm_oneclient

rpm_oneprovider: rpm_op_panel rpm_op_worker rpm_cluster_manager
	cp -f oneprovider_meta/oneprovider.spec.template oneprovider_meta/oneprovider.spec
	sed -i 's/{{scl}}/onedata$(RELEASE)/g' oneprovider_meta/oneprovider.spec
	sed -i 's/{{oneprovider_version}}/$(ONEPROVIDER_VERSION)/g' oneprovider_meta/oneprovider.spec
	sed -i 's/{{oneprovider_build}}/$(ONEPROVIDER_BUILD)/g' oneprovider_meta/oneprovider.spec
	sed -i 's/{{couchbase_version}}/$(COUCHBASE_VERSION)/g' oneprovider_meta/oneprovider.spec
	sed -i 's/{{cluster_manager_version}}/$(CLUSTER_MANAGER_VERSION)/g' oneprovider_meta/oneprovider.spec
	sed -i 's/{{op_worker_version}}/$(OP_WORKER_VERSION)/g' oneprovider_meta/oneprovider.spec
	sed -i 's/{{op_panel_version}}/$(OP_PANEL_VERSION)/g' oneprovider_meta/oneprovider.spec

	bamboos/docker/make.py -i onedata/rpm_builder:$(DISTRIBUTION)-$(RELEASE) \
		    -e DISTRIBUTION=$(DISTRIBUTION) -e RELEASE=$(RELEASE) --privileged --group mock -c \
	        mock --buildsrpm --spec oneprovider_meta/oneprovider.spec \
	        --sources oneprovider_meta --root $(DISTRIBUTION) \
	        --resultdir oneprovider_meta/package/packages

	bamboos/docker/make.py -i onedata/rpm_builder:$(DISTRIBUTION)-$(RELEASE) \
		    -e DISTRIBUTION=$(DISTRIBUTION) -e RELEASE=$(RELEASE) --privileged --group mock -c \
	        mock --rebuild oneprovider_meta/package/packages/*.src.rpm \
	        --root $(DISTRIBUTION) --resultdir oneprovider_meta/package/packages

	$(call mv_rpm, oneprovider_meta)

rpm_op_panel: clean_onepanel rpmdirs
	$(call make_rpm, onepanel, package) -e PKG_VERSION=$(OP_PANEL_VERSION) -e REL_TYPE=oneprovider
	$(call mv_rpm, onepanel)

rpm_op_worker: clean_op_worker rpmdirs
	$(call make_rpm, op_worker, package) -e PKG_VERSION=$(OP_WORKER_VERSION)
	$(call mv_rpm, op_worker)

rpm_cluster_manager: clean_cluster_manager rpmdirs
	$(call make_rpm, cluster_manager, package) -e PKG_VERSION=$(CLUSTER_MANAGER_VERSION)
	$(call mv_rpm, cluster_manager)

rpm_oneclient_base: clean_oneclient rpmdirs
	$(call make_rpm, oneclient, rpm) -e PKG_VERSION=$(ONECLIENT_VERSION)
	$(call mv_rpm, oneclient)

rpm_fsonedatafs: clean_fsonedatafs rpmdirs
	$(call make_rpm, fs-onedatafs, rpm) -e PKG_VERSION=$(FSONEDATAFS_VERSION) -e ONECLIENT_VERSION=$(ONECLIENT_VERSION)
	$(call mv_noarch_rpm, fs-onedatafs)

rpm_onedatafs_jupyter: clean_onedatafs_jupyter rpmdirs
	$(call make_rpm, onedatafs-jupyter, rpm) -e PKG_VERSION=$(ONEDATAFS_JUPYTER_VERSION) \
		                                     -e FSONEDATAFS_VERSION=$(FSONEDATAFS_VERSION) \
		                                     -e ONECLIENT_VERSION=$(ONECLIENT_VERSION)
	$(call mv_noarch_rpm, onedatafs-jupyter)

rpmdirs:
	mkdir -p package/$(DISTRIBUTION)/SRPMS package/$(DISTRIBUTION)/x86_64

##
## DEB packaging
##

deb: deb_oneprovider deb_oneclient

deb_oneprovider: deb_op_panel deb_op_worker deb_cluster_manager
	cp -f oneprovider_meta/oneprovider/DEBIAN/control.template oneprovider_meta/oneprovider/DEBIAN/control
	sed -i 's/{{oneprovider_version}}/$(ONEPROVIDER_VERSION)/g' oneprovider_meta/oneprovider/DEBIAN/control
	sed -i 's/{{oneprovider_build}}/$(ONEPROVIDER_BUILD)/g' oneprovider_meta/oneprovider/DEBIAN/control
	sed -i 's/{{couchbase_version}}/$(COUCHBASE_VERSION)/g' oneprovider_meta/oneprovider/DEBIAN/control
	sed -i 's/{{cluster_manager_version}}/$(CLUSTER_MANAGER_VERSION)/g' oneprovider_meta/oneprovider/DEBIAN/control
	sed -i 's/{{op_worker_version}}/$(OP_WORKER_VERSION)/g' oneprovider_meta/oneprovider/DEBIAN/control
	sed -i 's/{{op_panel_version}}/$(OP_PANEL_VERSION)/g' oneprovider_meta/oneprovider/DEBIAN/control
	sed -i 's/{{distribution}}/$(DISTRIBUTION)/g' oneprovider_meta/oneprovider/DEBIAN/control

	bamboos/docker/make.py -s oneprovider_meta -r . -c 'dpkg-deb -b oneprovider'
	mv oneprovider_meta/oneprovider.deb \
		package/$(DISTRIBUTION)/binary-amd64/oneprovider_$(ONEPROVIDER_VERSION)-$(ONEPROVIDER_BUILD)~$(DISTRIBUTION)_amd64.deb

deb_op_panel: clean_onepanel debdirs
	$(call make_deb, onepanel, package) -e PKG_VERSION=$(OP_PANEL_VERSION) \
		-e REL_TYPE=oneprovider -e DISTRIBUTION=$(DISTRIBUTION)
	$(call mv_deb, onepanel)

deb_op_worker: clean_op_worker debdirs
	$(call make_deb, op_worker, package) -e PKG_VERSION=$(OP_WORKER_VERSION) \
		-e DISTRIBUTION=$(DISTRIBUTION)
	$(call mv_deb, op_worker)

deb_cluster_manager: clean_cluster_manager debdirs
	$(call make_deb, cluster_manager, package) -e PKG_VERSION=$(CLUSTER_MANAGER_VERSION) \
		-e DISTRIBUTION=$(DISTRIBUTION)
	$(call mv_deb, cluster_manager)

deb_oneclient_base: clean_oneclient debdirs
	$(call make_deb, oneclient, deb) -e PKG_VERSION=$(ONECLIENT_VERSION)
	$(call mv_deb, oneclient)

deb_fsonedatafs: clean_fsonedatafs debdirs
	$(call make_deb, fs-onedatafs, deb) -e PKG_VERSION=$(FSONEDATAFS_VERSION) -e ONECLIENT_VERSION=$(ONECLIENT_VERSION)
	$(call mv_noarch_deb, fs-onedatafs)

deb_onedatafs_jupyter: clean_onedatafs_jupyter debdirs
	$(call make_deb, onedatafs-jupyter, deb) -e PKG_VERSION=$(ONEDATAFS_JUPYTER_VERSION) \
		                                     -e FSONEDATAFS_VERSION=$(FSONEDATAFS_VERSION) \
		                                     -e ONECLIENT_VERSION=$(ONECLIENT_VERSION)
	$(call mv_noarch_deb, onedatafs-jupyter)

debdirs:
	mkdir -p package/$(DISTRIBUTION)/source package/$(DISTRIBUTION)/binary-amd64

##
## Package artifact
##

package.tar.gz:
	tar -chzf package.tar.gz package

##
## Docker artifact
##

docker: docker-dev
	./docker_build.py --repository $(DOCKER_REG_NAME) --user $(DOCKER_REG_USER) \
                      --password $(DOCKER_REG_PASSWORD) \
                      --build-arg BASE_IMAGE=$(PROD_RELEASE_BASE_IMAGE) \
                      --build-arg RELEASE=$(RELEASE) \
                      --build-arg RELEASE_TYPE=$(DOCKER_RELEASE) \
                      --build-arg OP_PANEL_VERSION=$(OP_PANEL_VERSION) \
                      --build-arg COUCHBASE_VERSION=$(COUCHBASE_VERSION) \
                      --build-arg CLUSTER_MANAGER_VERSION=$(CLUSTER_MANAGER_VERSION) \
                      --build-arg OP_WORKER_VERSION=$(OP_WORKER_VERSION) \
                      --build-arg ONEPROVIDER_VERSION=$(ONEPROVIDER_VERSION) \
                      --build-arg HTTP_PROXY=$(HTTP_PROXY) \
                      --name oneprovider \
                      --publish --remove docker

docker-dev:
	./docker_build.py --repository $(DOCKER_REG_NAME) --user $(DOCKER_REG_USER) \
                      --password $(DOCKER_REG_PASSWORD) \
                      --build-arg BASE_IMAGE=$(DEV_RELEASE_BASE_IMAGE) \
                      --build-arg RELEASE=$(RELEASE) \
                      --build-arg OP_PANEL_VERSION=$(OP_PANEL_VERSION) \
                      --build-arg COUCHBASE_VERSION=$(COUCHBASE_VERSION) \
                      --build-arg CLUSTER_MANAGER_VERSION=$(CLUSTER_MANAGER_VERSION) \
                      --build-arg OP_WORKER_VERSION=$(OP_WORKER_VERSION) \
                      --build-arg ONEPROVIDER_VERSION=$(ONEPROVIDER_VERSION) \
                      --build-arg HTTP_PROXY=$(HTTP_PROXY) \
                      --report docker-dev-build-report.txt \
                      --short-report docker-dev-build-list.json \
                      --name oneprovider-dev \
                      --publish --remove docker

#
# Build intermediate Oneclient Docker image with oneclient installed from
# a normal (oneclient-base) package into /usr/ prefix.
#
docker_oneclient_base:
	$(MAKE) -C oneclient docker-base PKG_VERSION=$(ONECLIENT_VERSION) RELEASE=$(RELEASE) \
		                             FSONEDATAFS_VERSION=$(FSONEDATAFS_VERSION) \
		                             HTTP_PROXY=$(HTTP_PROXY)

#
# Build final Oneclient Docker image with oneclient installed from
# self contained package (oneclient) into /opt/oneclient prefix and
# symlinked into /usr prefix.
#
docker_oneclient:
	$(MAKE) -C oneclient docker PKG_VERSION=$(ONECLIENT_VERSION) RELEASE=$(RELEASE) \
                                FSONEDATAFS_VERSION=$(FSONEDATAFS_VERSION) \
                                HTTP_PROXY=$(HTTP_PROXY)

#
# Build Jupyter Docker with OnedataFS content manager plugin
#
docker_onedatafs_jupyter:
	$(MAKE) -C onedatafs-jupyter docker ONECLIENT_VERSION=$(ONECLIENT_VERSION) \
		                         ONEDATAFS_JUPYTER_VERSION=$(ONEDATAFS_JUPYTER_VERSION) \
		                         FSONEDATAFS_VERSION=$(FSONEDATAFS_VERSION) \
		                         HTTP_PROXY=$(HTTP_PROXY) \
		                         RELEASE=$(RELEASE)

#
# Build self-contained Oneclient archive, by extracting all necessary files
# from intermediate Oneclient Docker image (oneclient-base)
#
oneclient_tar oneclient/$(ONECLIENT_FPMPACKAGE_TMP)/oneclient-bin.tar.gz:
	$(MAKE) -C oneclient oneclient_tar

#
# Build production Oneclient RPM using FPM tool from self contained archive
#
oneclient_rpm: oneclient/$(ONECLIENT_FPMPACKAGE_TMP)/oneclient-bin.tar.gz rpmdirs
	$(MAKE) -C oneclient DISTRIBUTION=$(DISTRIBUTION) ONECLIENT_VERSION=$(ONECLIENT_VERSION) \
		oneclient_rpm
	mv oneclient/$(ONECLIENT_FPMPACKAGE_TMP)/oneclient*.rpm package/$(DISTRIBUTION)/x86_64

#
# Build production Oneclient DEB using FPM tool from self-contained archive
#
oneclient_deb: oneclient/$(ONECLIENT_FPMPACKAGE_TMP)/oneclient-bin.tar.gz debdirs
	$(MAKE) -C oneclient DISTRIBUTION=$(DISTRIBUTION) ONECLIENT_VERSION=$(ONECLIENT_VERSION) \
		oneclient_deb
	mv oneclient/$(ONECLIENT_FPMPACKAGE_TMP)/oneclient*.deb package/$(DISTRIBUTION)/binary-amd64

#
# Build and upload oneclient conda packages
#
oneclient_conda:
	$(call make_conda, oneclient, conda/oneclient) \
		-e CONDA_BUILD_OPTIONS="$(CONDA_BUILD_OPTIONS)" \
		-e PKG_VERSION=$(ONECLIENT_VERSION)

#
# Build and upload onedatafs conda packages
#
onedatafs_conda:
	$(call make_conda, oneclient, conda/onedatafs) \
		-e CONDA_BUILD_OPTIONS="$(CONDA_BUILD_OPTIONS)" \
		-e PKG_VERSION=$(ONECLIENT_VERSION)

#
# Build and upload fs.onedatafs conda packages
#
fsonedatafs_conda:
	$(call make_conda, fs-onedatafs, conda) \
		-e PKG_VERSION=$(FSONEDATAFS_VERSION) \
		-e CONDA_BUILD_OPTIONS="$(CONDA_BUILD_OPTIONS)" \
		-e ONECLIENT_VERSION=$(ONECLIENT_VERSION)

#
# Build and upload onedatafs-jupyter conda packages
#
onedatafs_jupyter_conda:
	$(call make_conda, onedatafs-jupyter, conda) \
		-e PKG_VERSION=$(ONEDATAFS_JUPYTER_VERSION) \
		-e CONDA_BUILD_OPTIONS="$(CONDA_BUILD_OPTIONS)" \
		-e FSONEDATAFS_VERSION=$(FSONEDATAFS_VERSION)


codetag-tracker:
	@echo "Skipping codetag-tracker for release version 20.02.*"

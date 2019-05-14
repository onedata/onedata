ARG BASE_IMAGE
FROM ${BASE_IMAGE}
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG RELEASE
ARG RELEASE_TYPE
ARG OP_PANEL_VERSION
ARG COUCHBASE_VERSION
ARG CLUSTER_MANAGER_VERSION
ARG OP_WORKER_VERSION
ARG ONEPROVIDER_VERSION
ARG ONEPANEL_AUTOSTART=false

# Add users and groups
RUN groupadd -g 150 onedata && \
    useradd -u 151 -g 150 -d /var/lib/op_panel op_panel && \
    useradd -u 152 -g 150 -d /var/lib/cluster_manager cluster_manager && \
    useradd -u 153 -g 150 -d /var/lib/op_worker op_worker

# Get the image up to date and install utility tools
RUN apt-get -y update && \
    apt-get -y upgrade && \
    apt-get -y install ca-certificates curl locales net-tools \
                       python python-setuptools python-yaml traceroute vim && \
    easy_install requests && \
    apt-get clean

# Generate locale
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# Install oneprovider package
RUN case ${RELEASE_TYPE} in \
        production) \
            curl -O http://get.onedata.org/oneprovider-${RELEASE}.sh; \
            ;; \
        *) \
            curl -O http://onedata-dev-packages.cloud.plgrid.pl/oneprovider-${RELEASE}.sh; \
            ;; \
    esac && \
    sh oneprovider-${RELEASE}.sh op-panel=${OP_PANEL_VERSION}-1 && \
    sh oneprovider-${RELEASE}.sh couchbase-server-community=${COUCHBASE_VERSION}-1 && \
    sh oneprovider-${RELEASE}.sh cluster-manager=${CLUSTER_MANAGER_VERSION}-1 && \
    sh oneprovider-${RELEASE}.sh op-worker=${OP_WORKER_VERSION}-1 && \
    sh oneprovider-${RELEASE}.sh oneprovider=${ONEPROVIDER_VERSION}-1 && \
    rm -f oneprovider-${RELEASE}.sh

# Backup files from persistence, as the persistent volume will be overwritten
# by mounting it from host. The missing files will be copied back in entrypoint.
ADD persistence-dir.py /root/persistence-dir.py
RUN python /root/persistence-dir.py --backup-persistent-files

# Create symlinks to persistence
RUN python /root/persistence-dir.py --create-symlinks

EXPOSE 80 443 6665 9443

# Add entrypoint scripts
ADD oneprovider.sh /root/oneprovider.sh
ADD oneprovider.py /root/oneprovider.py

RUN mkdir -p /volumes/storage
VOLUME ["/volumes/storage"]

CMD ["/root/oneprovider.sh"]

ARG BASE_IMAGE
FROM ${BASE_IMAGE}
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG RELEASE
ARG RELEASE_TYPE
ARG DISTRIBUTION=bionic
ARG OP_PANEL_VERSION
ARG CLUSTER_MANAGER_VERSION
ARG OP_WORKER_VERSION
ARG ONEPROVIDER_VERSION
ARG ONEPANEL_AUTOSTART=false
ARG HTTP_PROXY

# Install oneprovider package
RUN case ${RELEASE_TYPE} in \
        production) \
            curl -O http://packages.onedata.org/oneprovider-${RELEASE}.sh; \
            ;; \
        *) \
            curl -O http://packages.devel.onedata.org/oneprovider-${RELEASE}.sh; \
            ;; \
    esac && \
    sh oneprovider-${RELEASE}.sh op-panel=${OP_PANEL_VERSION}-1~${DISTRIBUTION} && \
    sh oneprovider-${RELEASE}.sh cluster-manager=${CLUSTER_MANAGER_VERSION}-1~${DISTRIBUTION} && \
    sh oneprovider-${RELEASE}.sh op-worker=${OP_WORKER_VERSION}-1~${DISTRIBUTION} && \
    sh oneprovider-${RELEASE}.sh oneprovider=${ONEPROVIDER_VERSION}-1~${DISTRIBUTION} && \
    rm -f oneprovider-${RELEASE}.sh

RUN mkdir -p /volumes/storage
VOLUME ["/volumes/storage"]

# Backup files from persistence, as the persistent volume will be overwritten
# by mounting it from host. The missing files will be copied back in entrypoint.
ADD persistence-dir.py /root/persistence-dir.py
RUN python /root/persistence-dir.py --backup-persistent-files

# Create symlinks to persistence
RUN python /root/persistence-dir.py --create-symlinks

# Add entrypoint scripts
ADD oneprovider.sh /root/oneprovider.sh
ADD oneprovider.py /root/oneprovider.py

# Disable http_proxy for normal operation
ENV http_proxy ""

EXPOSE 80 443 6665 9443

CMD ["/root/oneprovider.sh"]

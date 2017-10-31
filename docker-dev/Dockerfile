FROM onedata/worker:v54
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG COUCHBASE_VERSION
ARG CLUSTER_MANAGER_VERSION
ARG OP_PANEL_VERSION
ARG OP_WORKER_VERSION
ARG ONEPROVIDER_VERSION
ARG ONEPANEL_AUTOSTART=false

# Get the image up to date
RUN apt-get -y update && \
    apt-get -y upgrade && \
    apt-get clean

# Install oneprovider package
RUN curl -O http://onedata-dev-packages.cloud.plgrid.pl/oneprovider.sh && \
    sh oneprovider.sh couchbase-server-community=${COUCHBASE_VERSION}-1 && \
    sh oneprovider.sh cluster-manager=${CLUSTER_MANAGER_VERSION}-1 && \
    sh oneprovider.sh op-panel=${OP_PANEL_VERSION}-1 && \
    sh oneprovider.sh op-worker=${OP_WORKER_VERSION}-1 && \
    sh oneprovider.sh oneprovider=${ONEPROVIDER_VERSION}-1 && \
    rm -f oneprovider.sh

EXPOSE 53 53/udp 80 443 5555 5556 6665 6666 7443 8443 8876 8877 9443

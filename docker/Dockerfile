FROM ubuntu:16.04
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG RELEASE
ARG OP_PANEL_VERSION
ARG CLUSTER_MANAGER_VERSION
ARG OP_WORKER_VERSION
ARG ONEPROVIDER_VERSION
ARG ONEPANEL_AUTOSTART=false

# Add users and groups
RUN groupadd -g 110 onedata && \
    useradd -u 107 -g 110 -d /var/lib/op_panel op_panel && \
    useradd -u 108 -g 110 -d /var/lib/cluster_manager cluster_manager && \
    useradd -u 109 -g 110 -d /var/lib/op_worker op_worker

# Get the image up to date and install utility tools
RUN apt-get -y update && \
    apt-get -y upgrade && \
    apt-get -y install ca-certificates curl net-tools python python-setuptools \
                       python-yaml traceroute vim && \
    easy_install requests && \
    apt-get clean

# Generate locale
RUN locale-gen en_US.UTF-8

# Install oneprovider package
RUN case ${RELEASE} in \
        production) \
            curl -O http://get.onedata.org/oneprovider.sh; \
            ;; \
        *) \
            curl -O http://onedata-dev-packages.cloud.plgrid.pl/oneprovider.sh; \
            ;; \
    esac && \
    sh oneprovider.sh op-panel=${OP_PANEL_VERSION}-1; \
    sh oneprovider.sh cluster-manager=${CLUSTER_MANAGER_VERSION}-1; \
    sh oneprovider.sh op-worker=${OP_WORKER_VERSION}-1; \
    sh oneprovider.sh oneprovider=${ONEPROVIDER_VERSION}-1

EXPOSE 53 53/udp 80 443 5555 5556 6665 6666 7443 8443 8876 8877 9443

# Add entrypoint script
ADD oneprovider.py /root/oneprovider.py

RUN mkdir -p /volumes/persistence /volumes/storage
VOLUME ["/volumes/persistence", "/volumes/storage"]

CMD ["sh", "-c", "/root/oneprovider.py"]

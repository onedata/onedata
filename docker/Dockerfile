FROM onedata/fedora-systemd:23
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG RELEASE
ARG VERSION=""
ARG ONEPANEL_AUTOSTART=false

# Get the image up to date and install utility tools
RUN dnf -y upgrade && \
    dnf -y install fping ca-certificates python python-setuptools net-tools \
                   traceroute vim && \
    easy_install requests && \
    dnf clean all

# Install oneprovider package
RUN case ${RELEASE} in \
		production) \
			curl -o install.sh http://get.onedata.org/oneprovider.sh; \
                        sh install.sh ${VERSION}; \
			;; \
		*) \
			curl -o install.sh http://onedata-dev-packages.cloud.plgrid.pl/oneprovider.sh; \
                        sh install.sh ${VERSION}; \
			sed -i 's/{verify_oz_cert, true}/{verify_oz_cert, false}/g' /etc/op_panel/app.config \
			;; \
	esac

# Add missing CA bundle (required for server cert validation)
ADD ca-bundle.pem /etc/ssl/cert.pem

EXPOSE 53 5555 443 8443 80 8876 8877 9443

# Add entrypoint script
ADD run.py /root/run.py

RUN mkdir -p /volumes/persistence /volumes/storage
VOLUME ["/volumes/persistence", "/volumes/storage"]

CMD /root/run.py
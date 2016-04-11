FROM onedata/fedora-systemd:23
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG RELEASE
ARG VERSION=""
ARG ONEPANEL_AUTOSTART=false

# Get the image up to date
RUN dnf -y upgrade && \
    dnf clean all

# Install oneprovider package
RUN case ${RELEASE} in \
		production) \
			curl -o install.sh http://packages.onedata.org/install_oneprovider.sh; \
                        sh install.sh ${VERSION}; \
			;; \
		*) \
			curl -o install.sh http://onedata-dev-packages.cloud.plgrid.pl/install_oneprovider.sh; \
                        sh install.sh ${VERSION}; \
			sed -i 's/{verify_oz_cert, true}/{verify_oz_cert, false}/g' /etc/op_panel/app.config \
			;; \
	esac

# Add missing CA bundle (required for server cert validation)
RUN curl -o /etc/ssl/cert.pem https://raw.githubusercontent.com/bagder/ca-bundle/master/ca-bundle.crt

EXPOSE 53 5555 443 8443 80 8876 8877 9443

# Add entrypoint script
ADD run.sh /root/run.sh

CMD /root/run.sh

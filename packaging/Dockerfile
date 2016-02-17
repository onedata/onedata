FROM ubuntu:15.10
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG RELEASE
ARG ONEPANEL_AUTOSTART=false

# Get the image up to date
RUN apt-get update && \
    apt-get dist-upgrade -y && \
    apt-get clean

# Install required packages
RUN apt-get install -y curl sed

# Install oneprovider package
RUN case ${RELEASE} in \
		production) \
			curl -sSL http://packages.onedata.org/install_oneprovider.sh | sh \
			;; \
		*) \
			curl -sSL http://onedata-dev-packages.cloud.plgrid.pl/install_oneprovider.sh | sh; \
			sed -i 's/{verify_gr_cert, true}/{verify_gr_cert, false}/g' /etc/op_panel/app.config \
			;; \
	esac

# Add missing CA bundle (required for server cert validation)
RUN curl -o /etc/ssl/cert.pem https://raw.githubusercontent.com/bagder/ca-bundle/master/ca-bundle.crt

# Set up the environment
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

EXPOSE 53 5555 443 8443 80 8876 8877 9443

# Add entrypoint script
ADD run.sh /root/run.sh

CMD /root/run.sh

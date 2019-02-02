# A Docker image for XTA (C language) based on Debian Linux
FROM debian:9-slim
#
# Build with
#	docker build -t tiian/lixa-xta:1.7.3 .
# Execute with
#	docker run --rm -ti tiian/lixa-xta:1.7.3 bash
#
# Update Debian Software repository
ARG DEBIAN_FRONTEND=noninteractive
# Install generic pre-requisites
RUN apt-get update && apt-get install -y apt-utils
# Install run-time pre-requisites
RUN apt-get install -y libxml2 libglib2.0 uuid-runtime libpq5 --no-install-recommends
# build LIXA
ENV LIXA_DOWNLOAD_URL https://sourceforge.net/projects/lixa/files/latest/download
# @@@ temp
COPY lixa-1.7.3-dev.tar.gz /
# @@@ end temp
RUN BUILD_DEPS='curl make gcc pkg-config libxml2-dev libglib2.0-dev uuid-dev libpq-dev default-libmysqlclient-dev' \
	&& set -x \
	&& apt-get install -y $BUILD_DEPS --no-install-recommends \
	&& mkdir -p /lixa \
#	&& curl -sSL "$LIXA_DOWNLOAD_URL" -o lixa.tar.gz \
#	&& tar -xzf lixa.tar.gz -C /lixa \
# @@@ temp
	&& tar -xzf lixa-1.7.3-dev.tar.gz -C /lixa \
	&& rm /lixa-1.7.3-dev.tar.gz \
# @@@ end temp
#	&& rm lixa.tar.gz \
	&& cd /lixa/lixa-* \
	&& ./configure --disable-server --disable-syslog --disable-xta-cpp \
	--disable-xta-java --disable-xta-python --with-mysql --with-postgresql \
	&& make && make install \
	&& rm -rf /opt/lixa/share \
	&& rm -r /lixa \
	&& apt-get purge -y --auto-remove $BUILD_DEPS \
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/* \
	&& rm -rf /var/log/* \
	&& mv -v /opt/lixa/etc/lixac1_conf.xml /opt/lixa/etc/lixac_conf.xml \
	&& rm /opt/lixa/etc/lixad*_conf.xml
# security hardening: LIXA state server will run using non root privileges
#RUN addgroup --system lixa \
#	&& adduser --system --ingroup lixa --shell /bin/bash lixa \
#	&& chown -R lixa:lixa /opt/lixa/etc/ /opt/lixa/var/
# create an entry point
#COPY 	lixad-entrypoint.sh /home/lixa/
#RUN	chmod 540 /home/lixa/lixad-entrypoint.sh \
#	&& chown lixa:lixa /home/lixa/lixad-entrypoint.sh
# publish port 2345
#EXPOSE 2345
# basic trace messages: only MOD_SERVER and MOD_SERVER_LISTENER; see file
# src/common/lixa_trace.h for more available values
#ENV LIXA_TRACE_MASK 0x5
# start LIXA state server
#USER	lixa
#ENTRYPOINT ["/home/lixa/lixad-entrypoint.sh"]
#CMD 	["lixad"]


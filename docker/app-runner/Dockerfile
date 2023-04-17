# syntax=docker/dockerfile:1.4
FROM debian:stable-slim

RUN apt-get update && \
    apt-get install --no-install-recommends -y ca-certificates dpkg-dev libpq-dev libsodium-dev libsystemd-dev autoconf automake libtool pkg-config && \
    rm -rf /var/lib/apt/lists/*

COPY --from=secp256k1 /secp256k1 /opt/secp256k1

RUN cd /opt/secp256k1 && \
    ./autogen.sh && \
    ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
    make && \
    make check && \
    make install && \
    ldconfig

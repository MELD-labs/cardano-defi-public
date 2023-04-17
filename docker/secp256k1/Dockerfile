# syntax=docker/dockerfile:1.4
FROM alpine:latest

RUN apk add --no-cache git

RUN git clone https://github.com/bitcoin-core/secp256k1.git && \
    cd secp256k1 && \
    git reset --hard ac83be33d0956faf6b7f61a60ab524ef7d6a473a

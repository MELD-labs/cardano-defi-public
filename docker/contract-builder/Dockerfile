# syntax=docker/dockerfile:1.4
FROM haskell:9.2.4 AS builder-lib
RUN apt-get update && \
    apt-get install --no-install-recommends -y libsodium-dev libsystemd-dev autoconf automake libtool pkg-config r-base && \
    rm -rf /var/lib/apt/lists/*

# https://docs.docker.com/develop/develop-images/build_enhancements/#using-ssh-to-access-private-data-in-builds
RUN mkdir -p -m 0700 ~/.ssh && ssh-keyscan github.com >> ~/.ssh/known_hosts

RUN git clone https://github.com/bitcoin-core/secp256k1.git /opt/secp256k1 && \
    cd /opt/secp256k1 && \
    git reset --hard ac83be33d0956faf6b7f61a60ab524ef7d6a473a && \
    ./autogen.sh && \
    ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
    make && \
    make check && \
    make install

WORKDIR /defi

ARG TAG
ENV TAG=${TAG}

COPY build build

RUN --mount=type=cache,id=${TAG}-contract-dist-newstyle,target=/defi/dist-newstyle \
    --mount=type=cache,target=/root/.cabal \
    cabal --project-file=build/common.cabal.project update

COPY lending-contracts lending-contracts
COPY lending-types lending-types

RUN --mount=type=secret,id=gitconfig,readonly \
    git config --global include.path /run/secrets/gitconfig

FROM builder-lib AS installer-prod
COPY build/prod.cabal.project build/contract.cabal.project.local
RUN --mount=type=ssh \
    --mount=type=secret,id=gitconfig,readonly \
    --mount=type=cache,id=${TAG}-contract-dist-newstyle,target=/defi/dist-newstyle,sharing=locked \
    --mount=type=cache,target=/root/.cabal \
    build/contract.sh install --installdir=. --install-method=copy --overwrite-policy=always \
    lending-contracts-exporter

FROM builder-lib AS installer-dev
RUN --mount=type=ssh \
    --mount=type=secret,id=gitconfig,readonly \
    --mount=type=cache,id=${TAG}-contract-dist-newstyle,target=/defi/dist-newstyle,sharing=locked \
    --mount=type=cache,target=/root/.cabal \
    build/contract.sh build all && \
    cp \
    $(build/contract.sh list-bin --verbose=0 lending-contracts-exporter) \
    .

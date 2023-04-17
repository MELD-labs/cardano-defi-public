# syntax=docker/dockerfile:1.4
FROM haskell:8.10.7 AS builder-lib
RUN apt-get update && \
    apt-get install --no-install-recommends -y ca-certificates libsodium-dev libsystemd-dev autoconf automake libtool pkg-config && \
    rm -rf /var/lib/apt/lists/*

# https://docs.docker.com/develop/develop-images/build_enhancements/#using-ssh-to-access-private-data-in-builds
RUN mkdir -p -m 0700 ~/.ssh && ssh-keyscan github.com >> ~/.ssh/known_hosts

COPY --from=secp256k1 /secp256k1 /opt/secp256k1

RUN cd /opt/secp256k1 && \
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

RUN --mount=type=cache,id=${TAG}-app-dist-newstyle,target=/defi/dist-newstyle \
    --mount=type=cache,target=/root/.cabal \
    cabal --project-file=build/common.cabal.project update

COPY lending-api lending-api
COPY lending-api-docs lending-api-docs
COPY lending-bootstrap lending-bootstrap
COPY lending-core lending-core
COPY lending-faucet lending-faucet
COPY lending-index lending-index
COPY lending-services lending-services
COPY lending-mock-api lending-mock-api
COPY lending-oracle-service lending-oracle-service
COPY lending-tests lending-tests
COPY lending-types lending-types
COPY lending-liquidation-bot lending-liquidation-bot
COPY lending-migration lending-migration

RUN --mount=type=secret,id=gitconfig,readonly \
    git config --global include.path /run/secrets/gitconfig

FROM builder-lib AS installer-prod
COPY build/prod.cabal.project build/app.cabal.project.local
RUN --mount=type=ssh \
    --mount=type=secret,id=gitconfig,readonly \
    --mount=type=cache,id=${TAG}-app-dist-newstyle,target=/defi/dist-newstyle,sharing=locked \
    --mount=type=cache,target=/root/.cabal \
    build/app.sh install --installdir=. --install-method=copy --overwrite-policy=always \
    lending-api-exe lending-api-docs-app lending-bootstrap-deployer lending-bootstrap-initializer lending-index-app \
    lending-mock-api-exe lending-oracle-service-exe lending-services-exe lending-faucet-app \
    lending-liquidation-bot-liquidate lending-liquidation-bot-close-account lending-migration-exe && \
    build/app.sh build lending-integration-tests && cp $(build/app.sh list-bin --verbose=0 lending-integration-tests) .

FROM builder-lib AS installer-dev
RUN --mount=type=ssh \
    --mount=type=secret,id=gitconfig,readonly \
    --mount=type=cache,id=${TAG}-app-dist-newstyle,target=/defi/dist-newstyle,sharing=locked \
    --mount=type=cache,target=/root/.cabal \
    build/app.sh build all && \
    cp \
    $(build/app.sh list-bin --verbose=0 lending-api-exe) \
    $(build/app.sh list-bin --verbose=0 lending-api-docs-app) \
    $(build/app.sh list-bin --verbose=0 lending-bootstrap-deployer) \
    $(build/app.sh list-bin --verbose=0 lending-bootstrap-initializer) \
    $(build/app.sh list-bin --verbose=0 lending-index-app) \
    $(build/app.sh list-bin --verbose=0 lending-integration-tests) \
    $(build/app.sh list-bin --verbose=0 lending-faucet-app) \
    $(build/app.sh list-bin --verbose=0 lending-services-exe) \
    $(build/app.sh list-bin --verbose=0 lending-mock-api-exe) \
    $(build/app.sh list-bin --verbose=0 lending-oracle-service-exe) \
    $(build/app.sh list-bin --verbose=0 lending-liquidation-bot-liquidate) \
    $(build/app.sh list-bin --verbose=0 lending-liquidation-bot-close-account) \
    $(build/app.sh list-bin --verbose=0 lending-migration-exe) \
    .

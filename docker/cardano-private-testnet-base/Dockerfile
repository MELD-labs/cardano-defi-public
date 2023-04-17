###########################################################################
# WARNING: This image is publicly visible, so don't put MELD's code here. #
###########################################################################

FROM alpine:3
RUN apk add --no-cache bash coreutils curl jq

WORKDIR /private-testnet

# https://github.com/input-output-hk/cardano-node/releases.
ARG CARDANO_NODE_VERSION=1.35.5

# Fetch Cardano Node executables and expose them to path.
RUN mkdir tmp
RUN curl https://update-cardano-mainnet.iohk.io/cardano-node-releases/cardano-node-${CARDANO_NODE_VERSION}-linux.tar.gz | tar xz -C tmp
RUN mkdir bin && cp tmp/cardano-node tmp/cardano-cli bin && rm -rf tmp
ENV PATH "/private-testnet/bin:$PATH"

# Get the Cardano Node source code to run the private testnet from.
RUN curl https://codeload.github.com/input-output-hk/cardano-node/tar.gz/refs/tags/${CARDANO_NODE_VERSION} | tar xz --strip-components=1
RUN ./scripts/babbage/mkfiles.sh

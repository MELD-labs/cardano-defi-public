# syntax=docker/dockerfile:1.4
FROM contract-builder AS exporter

COPY config/local/lending-contracts-exporter.yaml config/local/lending-contracts-exporter.yaml
COPY config/preview/lending-contracts-exporter.yaml config/preview/lending-contracts-exporter.yaml
COPY config/preprod/lending-contracts-exporter.yaml config/preprod/lending-contracts-exporter.yaml

RUN mkdir /exported-contracts
ENV GHCRTS="-N4"

FROM exporter AS exporter-local
RUN mkdir -p /exported-contracts/local && \
    ./lending-contracts-exporter --config config/local/lending-contracts-exporter.yaml --output /exported-contracts/local

FROM exporter AS exporter-preview
RUN mkdir -p /exported-contracts/preview && \
    ./lending-contracts-exporter --config config/preview/lending-contracts-exporter.yaml --output /exported-contracts/preview

FROM exporter AS exporter-preprod
RUN mkdir -p /exported-contracts/preprod && \
    ./lending-contracts-exporter --config config/preprod/lending-contracts-exporter.yaml --output /exported-contracts/preprod

FROM exporter AS output
COPY --from=exporter-local /exported-contracts/local /exported-contracts/local
COPY --from=exporter-preview /exported-contracts/preview /exported-contracts/preview
COPY --from=exporter-preprod /exported-contracts/preprod /exported-contracts/preprod

# Upgrade to ply 0.3.1 to avoid this workaround
# See: https://github.com/mlabs-haskell/ply/blob/master/CHANGELOG.md#031---hotfix-for-integer-parameters
RUN sed -i 's/GHC.Num.Integer:Integer/GHC.Integer.Type:Integer/g' /exported-contracts/**/*.plutus

FROM scratch as summary
COPY --from=output /exported-contracts /

FROM output

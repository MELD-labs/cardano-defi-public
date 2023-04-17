# syntax=docker/dockerfile:1.4
# Contract exporter
FROM lending-contracts-exporter AS contract-exporter

# Builder
FROM app-builder AS builder

# Runner
FROM app-runner

WORKDIR /lending-index

COPY --from=builder defi/lending-index-app .

COPY --from=contract-exporter /exported-contracts /exported-contracts

COPY config/local/lending-index.yaml /config/local/lending-index.yaml
COPY config/preview/lending-index.yaml /config/preview/lending-index.yaml
COPY config/preprod/lending-index.yaml /config/preprod/lending-index.yaml

CMD ["./lending-index-app"]

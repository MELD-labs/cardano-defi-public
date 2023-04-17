# syntax=docker/dockerfile:1.4
# Contract exporter
FROM lending-contracts-exporter AS contract-exporter

# Builder
FROM app-builder AS builder

# Runner
FROM app-runner

WORKDIR /lending-bootstrap-initializer

COPY --from=builder defi/lending-bootstrap-initializer .

COPY --from=contract-exporter /exported-contracts /exported-contracts

COPY config/local/lending-contracts-exporter.yaml /config/local/lending-contracts-exporter.yaml
COPY config/preview/lending-contracts-exporter.yaml /config/preview/lending-contracts-exporter.yaml
COPY config/preprod/lending-contracts-exporter.yaml /config/preprod/lending-contracts-exporter.yaml

CMD ["./lending-bootstrap-initializer"]

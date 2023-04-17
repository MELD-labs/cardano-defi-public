# syntax=docker/dockerfile:1.4
# Contract exporter
FROM lending-contracts-exporter AS contract-exporter

# Builder
FROM app-builder AS builder

# Runner
FROM app-runner

WORKDIR /lending-oracle-service

COPY --from=builder defi/lending-oracle-service-exe .

COPY --from=contract-exporter /exported-contracts /exported-contracts

COPY config/local/lending-oracle-service.yaml /config/local/lending-oracle-service.yaml
COPY config/preview/lending-oracle-service.yaml /config/preview/lending-oracle-service.yaml
COPY config/preprod/lending-oracle-service.yaml /config/preprod/lending-oracle-service.yaml

CMD ["./lending-oracle-service-exe"]

# syntax=docker/dockerfile:1.4
# Contract exporter
FROM lending-contracts-exporter AS contract-exporter

# Builder
FROM app-builder AS builder

# Runner
FROM app-runner

WORKDIR /lending-services

COPY --from=builder defi/lending-services-exe .

COPY --from=contract-exporter /exported-contracts /exported-contracts

COPY config/local/lending-services.yaml /config/local/lending-services.yaml
COPY config/preview/lending-services.yaml /config/preview/lending-services.yaml
COPY config/preprod/lending-services.yaml /config/preprod/lending-services.yaml

CMD ["./lending-services-exe"]

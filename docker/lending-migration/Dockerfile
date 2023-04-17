# syntax=docker/dockerfile:1.4
# Contract exporter
FROM lending-contracts-exporter AS contract-exporter

# Builder
FROM app-builder AS builder

# Runner
FROM app-runner

WORKDIR /lending-migration

COPY --from=builder defi/lending-migration-exe .

COPY --from=contract-exporter /exported-contracts /exported-contracts

CMD ["./lending-migration-exe"]

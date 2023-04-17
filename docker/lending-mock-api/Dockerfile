# syntax=docker/dockerfile:1.4

# Builder
FROM app-builder AS builder

# Runner
FROM app-runner

WORKDIR /lending-mock-api

COPY --from=builder defi/lending-mock-api-exe .

COPY config/local/lending-oracle-service.yaml /config/local/lending-oracle-service.yaml
# COPY config/preview/lending-oracle-service.yaml /config/preview/lending-oracle-service.yaml
# COPY config/preprod/lending-oracle-service.yaml /config/preprod/lending-oracle-service.yaml

CMD ["./lending-mock-api-exe"]

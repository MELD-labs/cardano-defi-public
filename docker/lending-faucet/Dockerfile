# syntax=docker/dockerfile:1.4

# Builder
FROM app-builder AS builder

# Runner
FROM app-runner

WORKDIR /lending-faucet

COPY --from=builder defi/lending-faucet-app .

# Copy config
COPY config/local/lending-faucet.yaml /config/local/lending-faucet.yaml
COPY config/preview/lending-faucet.yaml /config/preview/lending-faucet.yaml
COPY config/preprod/lending-faucet.yaml /config/preprod/lending-faucet.yaml

CMD ["./lending-faucet-app"]

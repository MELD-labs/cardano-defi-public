# syntax=docker/dockerfile:1.4

# Builder
FROM app-builder AS builder

# Runner
FROM app-runner

WORKDIR /lending-api-docs

COPY --from=builder defi/lending-api-docs-app .

# Remove when we can configure environment in meld-infra
ENV API_HOST=localhost:3000
ENV FAUCET_API_HOST=localhost:3001

CMD ["./lending-api-docs-app"]

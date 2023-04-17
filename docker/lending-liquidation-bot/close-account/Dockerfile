# syntax=docker/dockerfile:1.4

# Builder
FROM app-builder AS builder

# Runner
FROM app-runner

WORKDIR /lending-liquidation-bot-close-account

COPY --from=builder defi/lending-liquidation-bot-close-account .

CMD ["./lending-liquidation-bot-close-account"]

# syntax=docker/dockerfile:1.4

# Builder
FROM app-builder AS builder

# Runner
FROM app-runner

WORKDIR /lending-liquidation-bot-liquidate

COPY --from=builder defi/lending-liquidation-bot-liquidate .

CMD ["./lending-liquidation-bot-liquidate"]

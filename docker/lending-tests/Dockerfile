# syntax=docker/dockerfile:1.4
FROM app-builder AS builder

FROM app-runner

WORKDIR /lending-tests

COPY --from=builder defi/lending-integration-tests .
COPY ./resources/functional-tests/ /resources/functional-tests

ENV TASTY_NUM_THREADS=4
ENV TASTY_COLOR=always

CMD ["./lending-integration-tests"]

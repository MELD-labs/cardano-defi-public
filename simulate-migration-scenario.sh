#!/usr/bin/env bash

export TAG=0.4.x

FUNCTIONAL_TEST="../resources/functional-tests" docker-compose \
  -f compose/docker-compose.local.yml \
  --profile test \
  up \
  --attach lending-tests && \

docker-compose \
  -f compose/docker-compose.local.yml \
  stop \
  lending-api \
  lending-api-docs \
  lending-index \
  lending-faucet \
  lending-mock-api \
  lending-oracle-service \
  lending-services

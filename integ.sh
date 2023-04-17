#!/usr/bin/env bash

docker-compose --profile test up --exit-code-from lending-tests --attach lending-tests --quiet-pull

#!/usr/bin/env bash

docker-compose --profile test --profile liquidation --profile migration down -v

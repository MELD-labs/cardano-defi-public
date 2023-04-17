#!/usr/bin/env bash

export TAG=${TAG:-latest}
export NETWORK=${NETWORK:-local}
export CARDANO_NODE_SOCKET_VOLUME=${CARDANO_NODE_SOCKET_VOLUME:-"defi-${NETWORK}_cardano-node-socket"}

echo "TAG: $TAG"
echo "NETWORK: $NETWORK"
echo "CARDANO_NODE_SOCKET_VOLUME: $CARDANO_NODE_SOCKET_VOLUME"
read -p "Migrate (y/n)? " -r choice
case "$choice" in
  y|Y|yes|Yes ) echo "yes";;
  * ) exit 1;;
esac

docker-compose -f compose/docker-compose.migration.yml up -d lending-migration

#!/usr/bin/env bash

_stop() {
  echo "Received SIGTERM or SIGINT signal"
  killall cardano-node
}

trap _stop TERM INT

# Use a common node socket file name with public testnets
mv /private-testnet/example/main.sock /private-testnet/example/node.socket

# We need to update the start times for the private testnet to work!
START_TIME_ISO=$(date -d "now + 2 seconds" -Iseconds)
START_TIME_UNIX=$(date -d "now + 2 seconds" +%s)
sed -E "s/(\"startTime\":) [0-9]+/\1 $START_TIME_UNIX/" -i example/genesis/byron/genesis.json
sed -E "s/(\"systemStart\":) \".*\"/\1 \"$START_TIME_ISO\"/" -i example/genesis/shelley/genesis.json
sed -E "s/(\"exUnitsMem\":) .*/\1 14000000,/" -i example/genesis/shelley/genesis.alonzo.json
example/run/all.sh &

wait

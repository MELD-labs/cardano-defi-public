#!/usr/bin/env bash

# Arguments
tx_out_args=()

for tx_out in "$@"; do
  # shellcheck disable=SC2206
  tx_out_args+=(--tx-out $tx_out)
done

# Useful variables
magic=42
utxo_keys_dir="./example/utxo-keys"

echo "Funding $*."

# Receive fund from a genesis utxo
utxo1_addr=$(cardano-cli genesis initial-addr --verification-key-file "$utxo_keys_dir/utxo1.vkey" --testnet-magic $magic | tr -d '\r')
tx_in=$(cardano-cli query utxo --testnet-magic $magic --address "$utxo1_addr" | awk 'FNR == 3 { print $1 "#" $2 }')

# Send fund to our payment address
cardano-cli transaction build \
  --babbage-era \
  --testnet-magic $magic \
  --tx-in "$tx_in" \
  "${tx_out_args[@]}" \
  --change-address "$utxo1_addr" \
  --out-file "$utxo_keys_dir/tx.raw"

# Sign the tx
cardano-cli transaction sign \
  --signing-key-file "$utxo_keys_dir/utxo1.skey" \
  --testnet-magic "$magic" \
  --tx-body-file "$utxo_keys_dir/tx.raw" \
  --out-file "$utxo_keys_dir/tx.signed"

# Print the tx id
echo "Transaction ID: $(cardano-cli transaction txid --tx-file "$utxo_keys_dir/tx.signed")"

# Submit the tx
cardano-cli transaction submit \
  --tx-file "$utxo_keys_dir/tx.signed" \
  --testnet-magic "$magic"

inputs=$(cardano-cli transaction view --tx-file "$utxo_keys_dir/tx.signed" | yq '.inputs[]' - | xargs -I {} echo -n " --tx-in " {})
echo "Waiting for transaction confirmation by querying inputs: $inputs"

max_retries=20
delay=1

for i in $(seq 1 $max_retries)
do
  echo "$inputs" | xargs cardano-cli query utxo --testnet-magic 42 --out-file input-utxos.json
  if [[ $(jq '.' input-utxos.json) == "{}" ]]; then
    exit 0
  else
    sleep $delay
    echo "Retrying #$i"
  fi
done

echo "Timed out"
exit 1

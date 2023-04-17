#!/usr/bin/env bash

network="$1"
deployerSkeyFile="$2"

# Extract network magic
networkMagic=()
if [ "$network" = mainnet ]; then
    networkMagic=("--mainnet")
elif [ "$network" = preview ]; then
    networkMagic=("--testnet-magic" "2")
elif [ "$network" = preprod ]; then
    networkMagic=("--testnet-magic" "1")
else
    echo "Do not support $1 network"
    exit 0
fi

# Get deployer's address
cardano-cli key verification-key --signing-key-file "${deployerSkeyFile}" --verification-key-file deployer.xvk
destinationAddress=$(cardano-cli address build --payment-verification-key-file deployer.xvk "${networkMagic[@]}")
echo destinationAddress: "$destinationAddress"

# Get balance UTxOs
cardano-cli query utxo --address "${destinationAddress}" "${networkMagic[@]}" --out-file fullUtxo.out
txIn=()
while IFS='' read -r txInUtxo; do
    txIn+=(--tx-in "$txInUtxo")
done < <(jq -r 'keys[]' fullUtxo.out)

# Prepare UTxOs for the deployer's wallet
cardano-cli transaction build \
    --babbage-era \
    "${networkMagic[@]}" \
    "${txIn[@]}" \
    --tx-out "${destinationAddress}"+100000000 \
    --tx-out "${destinationAddress}"+100000000 \
    --tx-out "${destinationAddress}"+100000000 \
    --tx-out "${destinationAddress}"+10000000 \
    --tx-out "${destinationAddress}"+10000000 \
    --tx-out "${destinationAddress}"+10000000 \
    --tx-out "${destinationAddress}"+10000000 \
    --tx-out "${destinationAddress}"+10000000 \
    --tx-out "${destinationAddress}"+10000000 \
    --tx-out "${destinationAddress}"+10000000 \
    --tx-out "${destinationAddress}"+10000000 \
    --change-address "${destinationAddress}" \
    --out-file tx.raw

# Sign the tx
cardano-cli transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file "${deployerSkeyFile}" \
    "${networkMagic[@]}" \
    --out-file tx.signed

# Print the tx id
echo "Transaction ID: $(cardano-cli transaction txid --tx-file tx.signed)"

# Submit the tx
cardano-cli transaction submit \
    --tx-file tx.signed \
    "${networkMagic[@]}"

# Remove the file that was created in the process.
rm -r fullUtxo.out deployer.xvk tx.raw tx.signed

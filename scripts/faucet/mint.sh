#!/usr/bin/env bash

utxo="dd60a4031ec169fc413860e9028f5270e58c82e545b2a1cac320f7072b0b5bb9#0"
recipient="addr_test1qrwa3utt9qadntfkj8z9nymrvfzkxst2uu4vqj76l4lq9pp4vlkf8lkkx3wrkfue2rmcgke8qdvd70n0udw5n0jy77ps8yht6h"
network="2"

policyid=$(cardano-cli transaction policyid --script-file faucet.policy)

tokens=(
  "1000000000 $policyid.744d454c44"
  "+1000000000 $policyid.74575254"
  "+1000000000 $policyid.74434f5049"
  "+1000000000 $policyid.744333"
  "+1000000000 $policyid.744d494e"
  "+1000000000 $policyid.74484f534b59"
  "+1000000000 $policyid.7469555344"
  "+1000000000 $policyid.7469425443"
  "+1000000000 $policyid.74574d54"
)

for ((i = 0; i < ${#tokens[@]}; i++))
do
  token=${tokens[$i]}
  mint+="$token"
done

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic $network \
  --tx-in "$utxo" \
  --mint-script-file faucet.policy \
  --mint "$mint" \
  --change-address "$(cat faucet.addr)" \
  --tx-out "$recipient+10000000+$mint" \
  --out-file faucet.tx

cardano-cli transaction sign --testnet-magic $network --signing-key-file faucet.xsk --tx-body-file faucet.tx --out-file faucet.tx.signed

cardano-cli transaction submit --testnet-magic $network --tx-file faucet.tx.signed

cardano-cli transaction txid --tx-file faucet.tx.signed

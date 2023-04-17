# Liquidation Bot

Because of the fast volatility of token prices, liquidation actions could fail.
Liquidation Bot helps advanced users to execute liquidate action and keep retrying if the action is fails.

## Running the Liquidation Bot

### Preparing

Before running the liquidation bot, users need to prepare the following things:

1. Docker installed
2. A fully-synced cardano node running in docker with a mounted volume.
3. An extended signing key file with token funded to its address. See [this link](https://developers.cardano.org/docs/stake-pool-course/handbook/keys-addresses/) to know how to generate keys and address using `cardano-cli`. To generate extended key, just put `--extended-key` at the end of the command.

### Running steps

1. Set the environment variable `NODE_VOLUME_NAME` with the volume name of the running cardano node.
2. Copy your signing key file to the root folder with the name `liquidator.xsk`.
   You can also use your current private key file by changing the file path in line 54 file `docker-compose.liquidation.yml`
3. Update `docker-compose.liquidation.yml`, turn the lending-index config files in to the right network (preview/preprod) at line 29 and 30.
4. Query the AccountId of the account you want to liquidate. You can query the liquidatable accounts list through the Lending API by:
   Preview:
   ```
   curl --location --request GET "https://api.lending.meldlabs.dev/lending/v1/account/exceed-ltv"
   ```
   Preprod:
   ```
   curl --location --request GET "https://staging-api.lending.meld.com/lending/v1/account/exceed-ltv"
   ```
5. Create a yaml config file, then mount to the docker image as `/config/local/lending-liquidation-bot.yaml` (update `docker-compose.liquidation.yaml` line 39)

   ```yaml
   # The delay time between retries
   lbRetryDelay: 10000000
   # The id of the account that you want to liquidate
   lbAccountId: a3000f07ad8cbdcef4a5ed4b19b75d9842e4dd0840f65b0677f2c428
   # Percent of extra repaid than the debt (The remaining number will be paid back after applied)
   lbRepayExtra: 0.1
   # Number of retry time
   lbMaxRetry: 10
   # The netowork and magic number of testnet (preprod: 1, preview: 2, private: 42)
   lbNetworkId:
     testnet: 42
   # The debt you want to liquidate
   # Format: asset: amount
   # You can query asset id using global state api
   lbLiquidatingDebt:
     0: 250000
   # The collateral you want to take
   # Format:
   # - If you want to custom the amount number of each asset, use the tag "Custom":
   lbLiquidatingCollateral:
     Custom:
       0: 270000 # Format: asset id : amount
   # - If you want to specify the assets, then the bot calculate the amount automatically, use the tag "PriorityOrder":
   # lbLiquidatingCollateral:
   #   PriorityOrder: # a list of asset's ids
   #      - 0
   #      - 1
   # You can query asset id using global state api
   ```

6. Run the lending-db and lending-index and wait some minute for the lending-index to complete syncing

   `docker compose -f docker-compose.liquidation.yml up lending-db lending-index -d`

7. Run the docker image
   `docker compose -f docker-compose.liquidation.yml up lending-liquidation-bot`

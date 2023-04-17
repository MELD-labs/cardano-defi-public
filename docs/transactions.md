# Contracts design

Lending & Borrowing includes the following contracts:

## Account contract

- Store account's data. Each utxo corresponds to an account.
- Validate user's interaction with an account.

## Account authentic token minting policy

- Identify authentic account.

## Pool contract

- Store all protocol's lending & borrowing data and contain all available tokens.
- Validate applying user's requests.

## Manager contract

- Store all protocol parameters.

## Oracle contract

- Provide tokens' prices.

# Transaction diagrams

## Create account

![image](images/create-account.jpeg)

## Update account

![image](images/update-account.jpeg)

## Close account

![image](images/close-account.jpeg)

## Liquidate

![image](images/liquidate.jpeg)

## Batching (Apply requests to Pool)

![image](images/batching.jpeg)

## Update Treasury

![image](images/update-treasury.jpeg)

## update Oracle

![image](images/update-oracle.jpeg)

## Update Manager

![image](images/update-manager.jpeg)

## Migrate Account

![image](images/migrate-account.jpeg)

## Migrate Pool

![image](images/migrate-pool.jpeg)

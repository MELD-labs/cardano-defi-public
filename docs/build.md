# Building and running the project

The project Haskell code requires two different versions of GHC:

- The smart contract code is written in [Plutarch](https://github.com/Plutonomicon/plutarch-plutus) which requires `ghc-9.2.4`. Consequently, we need `ghc-9.2.4` for contract builder.
- Requires `ghc-8.10.7` for app builder. `cardano-api` does not support GHC 9.x until recent.

## Building the project

### Installing Nix

We are using the [Nix Package Manager](https://nixos.org/download.html) to efficiently manage dependencies, especially in our case that uses different versions of ghc.

To install run the following command and follow the instructions:

```
sh <(curl -L https://nixos.org/nix/install) --daemon
```

### Setting up local dev envs

We are using `nix-direnv` to cache nix environment and fasten up the development.

Add the line use nix to your `.envrc` by:

```
echo "use nix" >> .envrc
direnv allow
```

For VS Code users, please do the following instructions to automatically apply the environment:

- Install [Nix Environment Selector](https://marketplace.visualstudio.com/items?itemName=arrterian.nix-env-selector).
- Open Command Palette (Ctrl + Shift + P) and run Nix-Env: Select Environment command.
- Choose `shell.nix`.
- Wait for the environment to build.
- Restart VS Code to apply the built environment.

### Build contract

Use `build/contract.sh` instead of `cabal`. For example, to update cabal repositories:

```
build/contract.sh update
```

To build all:

```
build/contract.sh build all
```

### Build applications

Use `build/app.sh` instead of `cabal`. For example, to update cabal repositories:

```
build/app.sh update
```

To build all:

```
build/app.sh build all
```

### Config HLS

Because we're building the project using 2 different versions of GHC, hence HLS requires 2 different builds. VS Code can only use one at the same time, so it's required to run before starting contract development in order to have HLS working correctly:

```
build/code/contract.sh
```

For application development:

```
build/code/app.sh
```

## Docker

### Build docker images

```
docker buildx bake
```

## Start docker dev services

```
docker-compose up -d
```

## Testing

### Run unit test and build image running integ tests

```
docker buildx bake test
```

### Run integ tests

```
docker-compose --profile test up --exit-code-from lending-tests --attach lending-tests
```

or

```
./integ.sh
```

### Run functional tests

```
./functional.sh
```

To add more test scenarios, create files inside folder `resource/functional-tests`.

### API doc

A Swagger UI is provided to describe the Lending API structure.

After starting the services, the Swagger UI runs on port 3002.

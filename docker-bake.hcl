variable "PRODUCTION" {
}

variable "TAG" {
  default = "latest"
}

variable "COMMIT_HASH" {
  default = ""
}

variable "REGISTRY" {
  default = "342832411065.dkr.ecr.eu-west-1.amazonaws.com"
}

variable "PUBLIC_REGISTRY" {
  default = "public.ecr.aws/z2l4u8l9"
}

variable "EXTERNAL_GIT_CONFIG" {
  default = ""
}

function "getSecret" {
  params = [secretKey, secretValue]
  result = notequal("",secretValue) ? "id=${secretKey},src=${secretValue}" : ""
}

variable "gitconfigSecrets" {
  default = [
    getSecret("gitconfig", EXTERNAL_GIT_CONFIG),
  ]
}

function "getTags" {
  params = [imageName]
  result = [
    notequal("",TAG) ? "${REGISTRY}/${imageName}:${TAG}" : "",
    notequal("",COMMIT_HASH) ? "${REGISTRY}/${imageName}:${COMMIT_HASH}" : "",
  ]
}

function "getPublicTags" {
  params = [imageName]
  result = [
    notequal("",TAG) ? "${PUBLIC_REGISTRY}/${imageName}:${TAG}" : "",
    notequal("",COMMIT_HASH) ? "${PUBLIC_REGISTRY}/${imageName}:${COMMIT_HASH}" : "",
  ]
}

group "builder" {
  targets = [
    "app-builder",
    "app-runner",
    "contract-builder",
  ]
}

group "default" {
  targets = [
    "lending-private-testnet",
    "lending-bootstrap-deployer",
    "lending-bootstrap-initializer",
    "lending-api",
    "lending-api-docs",
    "lending-index",
    "lending-services",
    "lending-mock-api",
    "lending-oracle-service",
    "lending-contracts-exporter-summary",
    "lending-faucet",
    "lending-liquidation-bot-liquidate",
    "lending-liquidation-bot-close-account",
    "lending-migration"
  ]
}

group "publish" {
  targets = [
    "lending-bootstrap-deployer",
    "lending-bootstrap-initializer",
    "lending-api",
    "lending-api-docs",
    "lending-index",
    "lending-services",
    "lending-mock-api",
    "lending-oracle-service",
    "lending-faucet",
    "lending-tests",
    "lending-liquidation-bot-liquidate",
    "lending-liquidation-bot-close-account",
    "lending-migration"
  ]
}

group "test" {
  targets = [
    "lending-private-testnet",
    "lending-bootstrap-deployer",
    "lending-bootstrap-initializer",
    "lending-api",
    "lending-api-docs",
    "lending-index",
    "lending-services",
    "lending-mock-api",
    "lending-oracle-service",
    "lending-contracts-exporter-summary",
    "lending-faucet",
    "lending-tests",
    "lending-liquidation-bot-liquidate",
    "lending-liquidation-bot-close-account",
    "lending-migration"
  ]
}

group "liquidate" {
  targets = [
    "lending-liquidation-bot-liquidate",
    "lending-liquidation-bot-close-account",
    "lending-index"
  ]
}

target "secp256k1" {
  dockerfile = "docker/secp256k1/Dockerfile"
}

target "cardano-private-testnet-base" {
  context = "docker/cardano-private-testnet-base/"
  tags = getPublicTags("cardano-private-testnet-base")
}

target "lending-private-testnet" {
  context = "docker/lending-private-testnet/"
  tags = getTags("lending-private-testnet")
}

target "contract-builder" {
  dockerfile = "docker/contract-builder/Dockerfile"
  target = "installer-${notequal("",PRODUCTION) ? "prod" : "dev"}"
  args = {
    "TAG": TAG
  }
  ssh = ["default"]
  secret = gitconfigSecrets
}

target "app-builder-lib" {
  dockerfile = "docker/app-builder/Dockerfile"
  target = "builder-lib"
  contexts = {
    secp256k1 = "target:secp256k1"
  }
  args = {
    "TAG": TAG
  }
  ssh = ["default"]
  secret = gitconfigSecrets
}

target "app-builder" {
  inherits = ["app-builder-lib"]
  target = "installer-${notequal("",PRODUCTION) ? "prod" : "dev"}"
}

target "app-runner" {
  dockerfile = "docker/app-runner/Dockerfile"
  contexts = {
    secp256k1 = "target:secp256k1"
    app-builder = "target:app-builder"
  }
}

target "lending-contracts-exporter" {
  dockerfile = "docker/lending-contracts-exporter/Dockerfile"
  contexts = {
    contract-builder = "target:contract-builder"
  }
}

target "lending-contracts-exporter-summary" {
  inherits = ["lending-contracts-exporter"]
  target = "summary"
  output = ["docker/out"]
}

target "app-base" {
  contexts = {
    app-builder = "target:app-builder"
    app-runner = "target:app-runner"
    lending-contracts-exporter = "target:lending-contracts-exporter"
  }
}

target "lending-bootstrap-deployer" {
  inherits = ["app-base"]
  dockerfile = "docker/lending-bootstrap-deployer/Dockerfile"
  tags = getTags("lending-bootstrap-deployer")
}

target "lending-bootstrap-initializer" {
  inherits = ["app-base"]
  dockerfile = "docker/lending-bootstrap-initializer/Dockerfile"
  tags = getTags("lending-bootstrap-initializer")
}

target "lending-api" {
  inherits = ["app-base"]
  dockerfile = "docker/lending-api/Dockerfile"
  tags = getTags("lending-api")
}

target "lending-api-docs" {
  inherits = ["app-base"]
  dockerfile = "docker/lending-api-docs/Dockerfile"
  tags = getTags("lending-api-docs")
}

target "lending-index" {
  inherits = ["app-base"]
  dockerfile = "docker/lending-index/Dockerfile"
  tags = getTags("lending-index")
}

target "lending-services" {
  inherits = ["app-base"]
  dockerfile = "docker/lending-services/Dockerfile"
  tags = getTags("lending-services")
}

target "lending-mock-api" {
  inherits = ["app-base"]
  dockerfile = "docker/lending-mock-api/Dockerfile"
  tags = getTags("lending-mock-api")
}

target "lending-oracle-service" {
  inherits = ["app-base"]
  dockerfile = "docker/lending-oracle-service/Dockerfile"
  tags = getTags("lending-oracle-service")
}

target "lending-unit-tests" {
  dockerfile = "docker/lending-unit-tests/Dockerfile"
  tags = getTags("lending-unit-tests")
  contexts = {
    contract-builder = "target:contract-builder"
    app-builder-lib = "target:app-builder-lib"
  }
  ssh = ["default"]
  secret = gitconfigSecrets
}

target "lending-tests" {
  inherits = ["app-base"]
  dockerfile = "docker/lending-tests/Dockerfile"
  tags = getTags("lending-tests")
}

target "lending-faucet" {
  inherits = ["app-base"]
  dockerfile = "docker/lending-faucet/Dockerfile"
  tags = getTags("lending-faucet")
}

target "lending-liquidation-bot-liquidate" {
  inherits = ["app-base"]
  dockerfile = "docker/lending-liquidation-bot/liquidate/Dockerfile"
  tags = getTags("lending-liquidation-bot-liquidate")
}

target "lending-liquidation-bot-close-account" {
  inherits = ["app-base"]
  dockerfile = "docker/lending-liquidation-bot/close-account/Dockerfile"
  tags = getTags("lending-liquidation-bot-close-account")
}

target "lending-migration" {
  inherits = ["app-base"]
  dockerfile = "docker/lending-migration/Dockerfile"
  tags = getTags("lending-migration")
}

target "remove-dist-newstyle" {
  dockerfile = "docker/ops/remove-dist-newstyle/Dockerfile"
  args = {
    "TAG": TAG
  }
}

target "erase-cabal-cache" {
  dockerfile = "docker/ops/erase-cabal-cache/Dockerfile"
}

{
  iohkNix ?
    import (
      builtins.fetchTarball "https://github.com/input-output-hk/iohk-nix/archive/e936cc0972fceb544dd7847e39fbcace1c9c00de.tar.gz"
    ) {},
  haskellNix ?
    import ( # Tracking 0.0.19
      builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/d6ce79f4b5350e4dd5b4b834b0a0e764cf16dff3.tar.gz"
    ) {},
  appHaskellCompilerVersion ? "8.10.7",
  contractHaskellCompilerVersion ? "9.2.4",
}:
let
  getCompilerName = version: pkgs.lib.strings.concatStrings ["ghc" (pkgs.lib.concatStringsSep "" (pkgs.lib.strings.splitString "." version))];
  contractHaskellCompiler = getCompilerName contractHaskellCompilerVersion;
  appHaskellCompiler = getCompilerName appHaskellCompilerVersion;
  pkgs = import haskellNix.sources.nixpkgs-unstable (
    haskellNix.nixpkgsArgs // {
      overlays = haskellNix.overlays ++ iohkNix.overlays.crypto;
    }
  );
in with pkgs; let
  devTools = [
    docker-compose
    git
    gnupg
    pre-commit
  ];
  getHls = compilerVersion:
    "ln -fs ${
      haskell-nix.tool (getCompilerName compilerVersion) "haskell-language-server" "1.8.0.0"
    }/bin/haskell-language-server $out/bin/haskell-language-server-${compilerVersion}";
  hlsWrapper = stdenv.mkDerivation {
    pname = "haskell-language-server-wrapper";
    version = pkgs.haskellPackages.haskell-language-server.version;
    buildCommand = ''
      mkdir -p $out/bin
      ln -fs ${pkgs.haskellPackages.haskell-language-server}/bin/haskell-language-server-wrapper $out/bin/haskell-language-server-wrapper
      ${getHls contractHaskellCompilerVersion}
      ${getHls appHaskellCompilerVersion}
    '';
  };
  haskellEnv = [
    hlsWrapper
    (haskell-nix.tool "${contractHaskellCompiler}" "hlint" "3.5")
    (haskell-nix.tool "${contractHaskellCompiler}" "fourmolu" "0.8.2.0")
  ];
  compilers = [
    haskell-nix.cabal-install.${contractHaskellCompiler}
    haskell-nix.compiler.${contractHaskellCompiler}
    haskell-nix.compiler.${appHaskellCompiler}
  ];
  libraries = [
    pkg-config
    libsodium-vrf
    secp256k1
    zlib # dependency: digest
    R # for newer version of plutus-core
    lzma # lending-api
    postgresql # lending-index
  ] ++ lib.optional (!stdenv.isDarwin) systemd;
  certFile = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
in
  mkShell {
    name = "lending";
    buildInputs = devTools ++ haskellEnv ++ compilers ++ libraries;
    GIT_SSL_CAINFO = certFile;
    NIX_SSL_CERT_FILE = certFile;
    COMPOSE_FILE="compose/docker-compose.local.yml";
    NETWORK="local";

    shellHook = ''
      pre-commit install --install-hooks -t pre-commit -t commit-msg
    '';
  }

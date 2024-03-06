{
  description = "tigerbeetle";

  inputs = {
    # nixpkgs.url = "github:nixos/nixpkgs";
    nixpkgs.url = "/home/shaun/nixpkgs";

    tigerbeetleSrc.url = "github:tigerbeetle/tigerbeetle";
    tigerbeetleSrc.flake = false;
  };

  outputs = { self, nixpkgs, tigerbeetleSrc, ... }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forallSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system:
          f (rec {
            inherit system;
            pkgs = nixpkgsFor system;
            haskellPackages = hpkgsFor system pkgs;
          }));
      nixpkgsFor = system: import nixpkgs { inherit system; };
      hpkgsFor = system: pkgs:
        let lib = pkgs.lib;
        in with pkgs.haskell.lib;
        pkgs.haskell.packages.ghc96.override {
          overrides = self: super: {
            haskell-language-server = dontCheck super.haskell-language-server;
            hls-semantic-tokens-plugin =
              dontCheck super.hls-semantic-tokens-plugin;
            ghcide = dontCheck super.ghcide;
          };
        };
    in {
      packages = forallSystems ({ system, pkgs, haskellPackages }: rec {
        tb_client = pkgs.stdenv.mkDerivation {
          name = "tb_client";
          src = tigerbeetleSrc;
          nativeBuildInputs = with pkgs; [ zig git ];
          buildPhase = ''
            ZIG_GLOBAL_CACHE_DIR=$(mktemp -d)
            export ZIG_GLOBAL_CACHE_DIR
            zig build -Drelease=true -Dcpu=baseline -Dgit-commit=${tigerbeetleSrc.rev} -Dversion=${tigerbeetleSrc.rev} c_client
          '';
          installPhase = ''
            mkdir -p $out/lib $out/include
            cp src/clients/c/lib/x86_64-linux-gnu/* $out/lib
            cp src/clients/c/lib/include/* $out/include
          '';
        };
        tigerbeetle = haskellPackages.callCabal2nix "tigerbeetle" ./. {
          inherit tb_client;
        };
        default = self.packages.${system}.tigerbeetle;
      });
      devShells = forallSystems ({ system, pkgs, haskellPackages }: {
        tigerbeetle = haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.tigerbeetle ];
          buildInputs = with haskellPackages; [
            cabal-install
            haskell-language-server
          ];
          withHoogle = true;
        };
        default = self.devShells.${system}.tigerbeetle;
      });
    };
}

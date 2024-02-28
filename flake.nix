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
        tigerbeetle = pkgs.stdenv.mkDerivation {
          name = "tigerbeetle";
          src = tigerbeetleSrc;
          nativeBuildInputs = with pkgs; [ zig.hook ];
          zigBuildFlags = [ ];
          dontUseZigCheck = true;
        };
        htigerbeetle = haskellPackages.callCabal2nix "tigerbeetle" ./. {
          inherit tigerbeetle;
        };
        default = self.packages.${system}.htigerbeetle;
      });
      devShells = forallSystems ({ system, pkgs, haskellPackages }: {
        htigerbeetle = haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.htigerbeetle ];
          buildInputs = with haskellPackages; [
            cabal-install
            haskell-language-server
          ];
          withHoogle = true;
        };
        default = self.devShells.${system}.htigerbeetle;
      });
    };
}

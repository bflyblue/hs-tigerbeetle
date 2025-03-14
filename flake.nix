{
  description = "tigerbeetle";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    tigerbeetleSrc.url = "github:tigerbeetle/tigerbeetle";
    tigerbeetleSrc.flake = false;
  };

  outputs =
    {
      self,
      nixpkgs,
      tigerbeetleSrc,
      ...
    }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forallSystems =
        f:
        nixpkgs.lib.genAttrs supportedSystems (
          system:
          f (rec {
            inherit system;
            pkgs = nixpkgsFor system;
            haskellPackages = hpkgsFor system pkgs;
          })
        );
      nixpkgsFor = system: import nixpkgs { inherit system; };
      hpkgsFor =
        system: pkgs:
        let
          lib = pkgs.lib;
        in
        with pkgs.haskell.lib;
        pkgs.haskell.packages.ghc98.override {
          overrides = self: super: {
            haskell-language-server = dontCheck super.haskell-language-server;
          };
        };
    in
    {
      packages = forallSystems (
        {
          system,
          pkgs,
          haskellPackages,
        }:
        rec {
          tb_client = pkgs.stdenv.mkDerivation {
            name = "tb_client";
            src = tigerbeetleSrc;
            nativeBuildInputs = with pkgs; [
              zig_0_13
              git
            ];
            buildPhase = ''
              ZIG_GLOBAL_CACHE_DIR=$(mktemp -d)
              export ZIG_GLOBAL_CACHE_DIR
              zig build --release -Dgit-commit=${tigerbeetleSrc.rev} clients:c
            '';
            installPhase = ''
              mkdir -p $out/lib $out/include
              cp src/clients/c/tb_client.h $out/include
              cp src/clients/c/lib/x86_64-linux-gnu*/* $out/lib
              patchelf --add-needed libm.so.6 $out/lib/libtb_client.so
            '';
          };
          tigerbeetle = haskellPackages.callCabal2nix "tigerbeetle" ./. {
            inherit tb_client;
          };
          default = self.packages.${system}.tigerbeetle;
        }
      );
      devShells = forallSystems (
        {
          system,
          pkgs,
          haskellPackages,
        }:
        {
          tigerbeetle = haskellPackages.shellFor {
            packages = p: [ self.packages.${system}.tigerbeetle ];
            buildInputs = with haskellPackages; [
              cabal-install
              haskell-language-server
            ];
            withHoogle = true;
          };
          default = self.devShells.${system}.tigerbeetle;
        }
      );
    };
}


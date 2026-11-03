{
  description = "Nixon - nix TUI interface";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/48652e9d5aea46e555b3df87354280d4f29cd3a3";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        nixonPackage = pkgs.haskellPackages.callCabal2nix "nixon" ./. {};

        devTools = with pkgs.haskellPackages; [
          ghc
          cabal-install
          haskell-language-server
        ];

        systemDeps = with pkgs; [
          zlib
          pkg-config
        ];
      in
        {
        packages.default = nixonPackage;

        apps.default = flake-utils.lib.mkApp {
          drv = nixonPackage;
          exePath = "/bin/nixon";
        };

        devShells.default = pkgs.mkShell {
          buildInputs = devTools ++ systemDeps;
        };
      });
}

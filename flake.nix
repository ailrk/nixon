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

        haskellPackages = pkgs.haskellPackages.extend (self: super: {
          gum-brick = self.callCabal2nix "gum-brick" ./gum-brick {};
          nixon = self.callCabal2nix "nixon" ./nixon {};
        });

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
        packages.default = haskellPackages.nixon;

        apps.default = flake-utils.lib.mkApp {
          drv = haskellPackages.nixon;
          exePath = "/bin/nixon";
        };

        devShells.default = pkgs.mkShell {
          buildInputs = devTools ++ systemDeps;
        };
      });
}

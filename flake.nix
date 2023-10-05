{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, flake-utils, ... }:
    let
      pkgs = import ./nix/pkgs.nix {
        inherit inputs;
        system = "x86_64-linux";
      };
      packageName = "astro-stacker";
    in {
      packages.x86_64-linux.${packageName} = pkgs.haskellPackages.astro-stacker;
      defaultPackage.x86_64-linux = pkgs.haskell.lib.justStaticExecutables
        self.packages.x86_64-linux.${packageName};
      devShells.x86_64-linux = {
        default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.astro-stacker ];
          nativeBuildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.ghc
            haskellPackages.hlint
            haskellPackages.ghcid
            haskellPackages.haskell-language-server
            haskellPackages.fourmolu
          ];
        };
      };
    };
}

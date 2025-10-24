{ inputs }:
let
  customHaskellPackages = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper:
        let
          dontCheck = super.haskell.lib.dontCheck;
          dontHaddock = super.haskell.lib.dontHaddock;
          doJailbreak = super.haskell.lib.compose.doJailbreak;

          repa = doJailbreak hsuper.repa;
          type-errors = doJailbreak hsuper.type-errors;
          ghc-prof = doJailbreak hsuper.ghc-prof;
          profiteur = doJailbreak hsuper.profiteur;

          astro-stacker-src = self.nix-gitignore.gitignoreSource [
            "*.git"
            "dist"
            "dist-newstyle"
          ] ../.;
          astro-stacker = dontHaddock
            (hself.callCabal2nix "astro-stacker" astro-stacker-src { });
        in {
          # We add ourselves to the set of haskellPackages.
          inherit astro-stacker;
          inherit repa type-errors ghc-prof profiteur;
        };
    };
  };
in [
  # (final: prev: { haskellPackages = prev.haskell.packages.ghc98; })
  customHaskellPackages
]

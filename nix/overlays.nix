{ inputs }:
let
  customHaskellPackages = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper:
        let
          dontCheck = super.haskell.lib.dontCheck;
          dontHaddock = super.haskell.lib.dontHaddock;

          astro-stacker-src = self.nix-gitignore.gitignoreSource [
            "*.git"
            "dist"
            "dist-newstyle"
          ] ../.;
          astro-stacker =
            dontHaddock (hself.callCabal2nix "astro-stacker" astro-stacker-src { });
        in {
          # We add ourselves to the set of haskellPackages.
          inherit astro-stacker;
        };
    };
  };
in [ customHaskellPackages ]

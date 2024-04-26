{ inputs, system }:
let
  overlays = import ./overlays.nix { inherit inputs; };
  config = { allowBroken = false; };
in import inputs.nixpkgs { inherit config overlays system; }

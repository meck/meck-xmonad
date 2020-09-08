{ scaling ? 1.0 }:
let

  pkgs = import ./nixpkgs.nix;

in pkgs.callPackage ./derivation.nix {
  pkgs = pkgs;
  scaling = scaling;
}

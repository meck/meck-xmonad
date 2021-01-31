{ usePinned ? false, scaling ? 1.0 }:
let

  pkgs = if usePinned then import ./nixpkgs.nix else import <nixpkgs> { };

in pkgs.callPackage ./derivation.nix {
  pkgs = pkgs;
  scaling = scaling;
}

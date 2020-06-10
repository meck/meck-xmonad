{ scaling ? 1.0 }:
let
  nixpkgs = import ./nixpkgs.nix;

  pkgs = import nixpkgs { config = { }; };

  compiler = "ghc883";

in pkgs.callPackage ./derivation.nix {
  pkgs = pkgs;
  compiler = compiler;
  scaling = scaling;
}

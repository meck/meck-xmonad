let
  nixpkgs = builtins.fetchTarball {
    name = "nixos-unstable-2020-09-08";
    url =
      "https://github.com/nixos/nixpkgs/archive/5aba0fe9766a7201a336249fd6cb76e0d7ba2faf.tar.gz";
    sha256 = "05gawlhizp85agdpw3kpjn41vggdiywbabsbmk76r2dr513188jz";
  };
in (import nixpkgs) { overlays = [ (import ./overlay.nix) ]; }

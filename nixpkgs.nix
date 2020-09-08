let
  nixpkgs = builtins.fetchTarball {
    name = "nixos-unstable-2020-09-08";
    url =
      "https://github.com/nixos/nixpkgs/archive/3c0e3697520cbe7d9eb3a64bfd87de840bf4aa77.tar.gz";
    sha256 = "1vx7kyaq0i287dragjgfdj94ggwr3ky2b7bq32l8rkd2k3vc3gl5";
  };
in (import nixpkgs) { overlays = [ (import ./overlay.nix) ]; }

let
  nixpkgs = builtins.fetchTarball {
    name = "nixos-unstable-2020-10-25";
    url =
      "https://github.com/nixos/nixpkgs/archive/2deeb58f49480f468adca6b08291322de4dbce6b.tar.gz";
    sha256 = "0fx2car6dcd1yz6jjkifcan0amwzhs3170h0r69k0wfwiaadpvjv";
  };
in (import nixpkgs) { overlays = [ (import ./overlay.nix) ]; }

# taffybar is broken in 20.03

let
  nixpkgs = builtins.fetchTarball {
    name = "nixos-unstable-2020-05-29";
    url =
      "https://github.com/nixos/nixpkgs/archive/b27a19d5bf799f581a8afc2b554f178e58c1f524.tar.gz";
    sha256 = "0xl67j7ns9kzk1arr64r4lfiq74nw0awqbv6hnh8njx07rspqhdb";
  };
in nixpkgs

let
  nixpkgs = builtins.fetchTarball {
    name = "nixos-unstable-2020-10-25";
    url =
      "https://github.com/nixos/nixpkgs/archive/2247d824fe07f16325596acc7faa286502faffd1.tar.gz";
    sha256 = "09jzdnsq7f276cfkmynqiaqg145k8z908rlr0170ld1sms1a83rw";
  };
in (import nixpkgs) { overlays = [ (import ./overlay.nix) ]; }

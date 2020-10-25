let
  nixpkgs = builtins.fetchTarball {
    name = "nixos-unstable-2020-10-25";
    url =
      "https://github.com/nixos/nixpkgs/archive/bd017760509492a1774b0ea2fc5c1bbb4b2d98af.tar.gz";
    sha256 = "0rd7dc677s76vham7mjfc7286lfigi0ynx32ls14ffi7xwy9js42";
  };
in (import nixpkgs) { overlays = [ (import ./overlay.nix) ]; }

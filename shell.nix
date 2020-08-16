{ compiler ? "ghc883" }:

let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs { };
  myHaskellPackages = pkgs.haskell.packages.${compiler};

  nixpkgs_with_hls = builtins.fetchTarball {
    name = "nixos-unstable-2020-08-10";
    url =
      "https://github.com/nixos/nixpkgs/archive/32b46dd897ab2143a609988a04d87452f0bbef59.tar.gz";
    sha256 = "1gzfrpjnr1bz9zljsyg3a4zrhk8r927sz761mrgcg56dwinkhpjk";
  };

  hls = (import nixpkgs_with_hls
    { }).haskell.packages.${compiler}.haskell-language-server;

  drv = pkgs.callPackage ./derivation.nix {
    pkgs = pkgs;
    compiler = compiler;
    scaling = 1.0;
  };

in myHaskellPackages.shellFor {
  packages = pkgs: [ drv myHaskellPackages.cabal-install ];
  withHoogle = true;
  nativeBuildInputs = with myHaskellPackages; [ hls cabal-install ];
}

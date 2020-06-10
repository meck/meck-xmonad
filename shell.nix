# taffybar is broken in 20.03
{ compiler ? "ghc883" }:

let
  nixpkgs = import ./nixpkgs.nix;

  pkgs = import nixpkgs { config = { }; };

  myHaskellPackages = pkgs.haskell.packages.${compiler};
  drv = pkgs.callPackage ./derivation.nix {
    pkgs = pkgs;
    compiler = compiler;
    scaling = 1.0;
  };
  all-hies = import
    (fetchTarball "https://github.com/infinisil/all-hies/tarball/haskell.nix")
    { };

  # all-hies = import (builtins.fetchTarball {
  #   name = "all-hies-2020-06-01";
  #   url =
  #     "https://github.com/infinisil/all-hies/archive/4d52f70a28b337a70d1c92f5a4483ebcd7612e03.tar.gz";
  #   sha256 = "0g7bmzx2ifij87j32kvbmy1gxcyqiawwafra6l027n70kx5hbfva";
  # }) { };

  hie = (all-hies.selection { selector = p: { inherit (p) "ghc883"; }; });

in myHaskellPackages.shellFor {
  packages = pkgs: [ drv myHaskellPackages.cabal-install ];
  withHoogle = true;
  nativeBuildInputs = with myHaskellPackages; [
    hie
    ormolu
    brittany
    weeder
    cabal-install
  ];

}

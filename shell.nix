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

  # all-hies = import
  #   (fetchTarball "https://github.com/infinisil/all-hies/tarball/haskell.nix")
  #   { };

  # hie = (all-hies.selection { selector = p: { inherit (p) "ghc883"; }; });

  nix-hls = import (pkgs.fetchFromGitHub {
      owner = "poscat0x04";
      repo = "hls-nix";
      rev = "df53e76a882da8792c44f9fe337fa994ffb29a48";
      sha256 = "13phk3p7qkwb8mlzqzncxdxmabkswh7hn8h7lqxaxrkhxrv8h5j6";
  });

  hls = (nix-hls { tag = "master"; version = "8.8.3";}).exes.haskell-language-server;

in myHaskellPackages.shellFor {
  packages = pkgs: [ drv myHaskellPackages.cabal-install ];
  withHoogle = true;
  nativeBuildInputs = with myHaskellPackages; [
    # hie
    hls
    ormolu
    brittany
    weeder
    cabal-install
  ];

}

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

  nix-hls = import (pkgs.fetchFromGitHub {
      owner = "poscat0x04";
      repo = "hls-nix";
      rev = "772f7e76950bf871263898e02c1c2863375ae5a1";
      sha256 = "0a0wc5di71s0y38zv7ilrwqfmapv14hd4a47545jvzsv96p9g763";
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

{ usePinned ? false, compiler ? "default" }:
let

  pkgs = if usePinned then import ./nixpkgs.nix else import <nixpkgs> { };

  myHaskellPkgs = if compiler == "default" then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  drv = pkgs.callPackage ./derivation.nix {
    pkgs = pkgs;
    compiler = compiler;
    scaling = 1.0;
  };

in myHaskellPkgs.shellFor {
  packages = pkgs: [ drv myHaskellPkgs.cabal-install ];
  withHoogle = true;
  nativeBuildInputs = with myHaskellPkgs; [
    haskell-language-server
    cabal-install
  ];
}

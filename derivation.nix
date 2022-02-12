{ pkgs, compiler ? "default" }:
let

  myHaskellPkgs =
    if compiler == "default" then
      pkgs.haskellPackages
    else
      pkgs.haskell.packages.${compiler};

  drv = myHaskellPkgs.callCabal2nix "meck-xmonad" (builtins.fetchGit ./.) { };

in
pkgs.haskell.lib.overrideCabal drv (old: { })

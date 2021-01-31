{ pkgs, compiler ? "default", scaling }:
let

  myHaskellPkgs = if compiler == "default" then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  drv = myHaskellPkgs.callCabal2nix "meck-xmonad" (builtins.fetchGit ./.) { };

in pkgs.haskell.lib.overrideCabal drv (old: {

  # Change xmonad.hs to support hidpi
  postConfigure =
    "substituteInPlace exe/xmonad.hs --replace 'resScaling = 1.0' 'resScaling = ${
      toString scaling
    }'";

})

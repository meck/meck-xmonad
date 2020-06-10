{ pkgs ? import <nixpkgs> { }, compiler ? "ghc883", scaling }:
let

  haskellPackages = pkgs.haskell.packages.${compiler};

  # Version with my fix for struts in hidpi
  # https://github.com/taffybar/gtk-strut/pull/5
  gtk-strut_from_git = pkgs.fetchgit {
    url = "https://github.com/taffybar/gtk-strut.git";
    rev = "cc8d4608c26f9989cc3879bbbc6c2835b9648015";
    sha256 = "0ccg8fi262cy0hw2pahciaw05bycl9lav01bw4rqgjwiyhbkxnpa";
  };

  # for working on TB
  localTaffbar = builtins.fetchGit (/home/meck/taffybar);


  myHaskellPkgs = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      (self: super: {
        gtk-strut = self.callCabal2nix "gtk-strut" gtk-strut_from_git { };

        # taffybar = (self.callCabal2nix "taffybar" localTaffbar) {inherit (pkgs) gtk3;};
      });
  });

  drv = myHaskellPkgs.callCabal2nix "meck-xmonad" (builtins.fetchGit ./.) { };

in pkgs.haskell.lib.overrideCabal drv (old: {

  # Change xmonad.hs to sopport
  # hidpi
  postConfigure =
    "substituteInPlace exe/xmonad.hs --replace 'resScaling = 1.0' 'resScaling = ${
      toString scaling
    }'";

  # taffybar css file
  postInstall = "cp misc/taffybar.css $out/taffybar.css";
})

self: super: {

  haskellPackages = with self.haskell.lib;
    super.haskellPackages.extend (hself: hsuper: {

      # Version with my fix for struts in hidpi
      # https://github.com/taffybar/gtk-strut/pull/5
      gtk-strut = self.haskellPackages.callCabal2nix "gtk-strut"
        (self.fetchgit {
          url = "https://github.com/taffybar/gtk-strut.git";
          rev = "cc8d4608c26f9989cc3879bbbc6c2835b9648015";
          sha256 = "0ccg8fi262cy0hw2pahciaw05bycl9lav01bw4rqgjwiyhbkxnpa";
        }) { };

      taffybar = markUnbroken (appendPatch hsuper.taffybar (self.fetchpatch {
        url =
          "https://github.com/taffybar/taffybar/pull/494/commits/a7443324a549617f04d49c6dfeaf53f945dc2b98.patch";
        sha256 = "0prskimfpapgncwc8si51lf0zxkkdghn33y3ysjky9a82dsbhcqi";
      }));
    });
}

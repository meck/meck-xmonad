# Mostly from
# https://github.com/NixOS/nixpkgs/issues/63500#issuecomment-674835273
self: super: {

  haskellPackages = with self.haskell.lib;
    super.haskellPackages.extend (hself: hsuper: {

      gi-cairo-render = markUnbroken (hself.gi-cairo-render_0_1_0);

      gi-dbusmenu = markUnbroken (hself.gi-dbusmenu_0_4_8);

      gi-dbusmenugtk3 = markUnbroken (hself.gi-dbusmenugtk3_0_4_9);

      gi-gdkx11 = markUnbroken (overrideSrc hsuper.gi-gdkx11 {
        src = self.fetchurl {
          url =
            "https://hackage.haskell.org/package/gi-gdkx11-3.0.10/gi-gdkx11-3.0.10.tar.gz";
          sha256 = "0kfn4l5jqhllz514zw5cxf7181ybb5c11r680nwhr99b97yy0q9f";
        };
        version = "3.0.10";
      });

      gi-gtk-hs = markUnbroken (hself.gi-gtk-hs_0_3_9);

      gi-cairo-connector = markUnbroken (hself.gi-cairo-connector_0_1_0);

      gi-xlib = markUnbroken (hself.gi-xlib_2_0_9);

      gtk-sni-tray = markUnbroken (hsuper.gtk-sni-tray);

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

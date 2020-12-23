self: super: {

  haskellPackages = with self.haskell.lib;
    super.haskellPackages.extend (hself: hsuper: {

      taffybar = markUnbroken hsuper.taffybar;
    });
}

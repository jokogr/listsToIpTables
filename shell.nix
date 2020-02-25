let
  pkgs = import ./nix {};
in pkgs.haskellPackages.shellFor
  {
    packages = p: [ p.listsToIpTables ];
    withHoogle = false;
    buildInputs = [ pkgs.cabal-install pkgs.ipset pkgs.niv pkgs.ormolu ];
  }

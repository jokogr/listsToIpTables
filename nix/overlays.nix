{ sources }:
[
  # Some extra sources
  (self: super:
    { haskellPackages = super.haskellPackages.extend
          (super.haskell.lib.packageSourceOverrides
            { listsToIpTables = self.lib.cleanSourceWith
                { filter = name: type:
                    (name == builtins.toString ../Main.hs) ||
                    (name == builtins.toString ../listsToIpTables.cabal) ||
                    (name == builtins.toString ../README.md) ||
                    (name == builtins.toString ../LICENSE) ||
                    (name == builtins.toString ../Setup.hs)
                    ;

                  src = self.lib.cleanSource ../.;
                };
              niv = (import sources.niv { pkgs = super; }).niv-source;
            }
          );
    }
  )
]

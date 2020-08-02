{
  description = "A flake for listToIptables";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-20.03;
  };

  outputs = { self, nixpkgs, ... }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in
    {

      overlay = final: prev: {
        listsToIptables =
	  let
            modpkgs = import nixpkgs { system = "x86_64-linux"; overlays = import ./nix/overlays.nix { sources = import ./nix/sources.nix; }; };
          in
            modpkgs.haskellPackages.listsToIpTables;
      };

      defaultPackage = forAllSystems (system: (import nixpkgs {
	inherit system;
        overlays = [ self.overlay ];
      }).listsToIptables);

    };
}

{ sources ? import ./sources.nix }:

let
  pkgs = import sources.nixpkgs { };

in
  pkgs.haskellPackages.callPackage ./liststoiptables.nix { }

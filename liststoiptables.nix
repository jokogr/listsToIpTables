{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "ListsToIpTables";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  description = "Download IP lists and transform them to IP sets";
  license = stdenv.lib.licenses.gpl3;
}

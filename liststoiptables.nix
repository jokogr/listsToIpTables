{ mkDerivation, base, bytestring, HTTP, iproute, network-uri
, stdenv, zlib
}:
mkDerivation {
  pname = "ListsToIpTables";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring HTTP iproute network-uri zlib
  ];
  description = "Download IP lists and transform them to IP sets";
  license = stdenv.lib.licenses.gpl3;
}

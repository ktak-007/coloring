{ mkDerivation, base, bytestring, conduit, lib, parsec, rainbow
, text
}:
mkDerivation {
  pname = "coloring";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring conduit parsec rainbow text
  ];
  executableHaskellDepends = [
    base bytestring conduit parsec rainbow text
  ];
  testHaskellDepends = [
    base bytestring conduit parsec rainbow text
  ];
  homepage = "https://github.com/githubuser/coloring#readme";
  license = lib.licenses.bsd3;
  mainProgram = "coloring";
}

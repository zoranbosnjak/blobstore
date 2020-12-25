{ mkDerivation, base, base16-bytestring, bytestring, cryptonite
, exceptions, flock, foldl, memory, monad-control
, optparse-applicative, stdenv, temporary, text, turtle
}:
mkDerivation {
  pname = "blobstore";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base16-bytestring bytestring cryptonite exceptions flock foldl
    memory monad-control optparse-applicative temporary text turtle
  ];
  executableHaskellDepends = [
    base base16-bytestring bytestring cryptonite exceptions flock foldl
    memory monad-control optparse-applicative temporary text turtle
  ];
  description = "BLOB store on disk";
  license = stdenv.lib.licenses.bsd3;
}

{ mkDerivation, base, base16-bytestring, bytestring, exceptions
, flock, foldl, lib, monad-control, optparse-applicative, process
, temporary, text, turtle
}:
mkDerivation {
  pname = "blobstore";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base16-bytestring bytestring exceptions flock foldl
    monad-control optparse-applicative process temporary text turtle
  ];
  executableHaskellDepends = [
    base base16-bytestring bytestring exceptions flock foldl
    monad-control optparse-applicative process temporary text turtle
  ];
  description = "BLOB store on disk";
  license = lib.licenses.bsd3;
}

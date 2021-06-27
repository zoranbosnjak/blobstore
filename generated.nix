{ mkDerivation, base, base16-bytestring, bytestring, exceptions
, flock, foldl, lib, monad-control, optparse-applicative, process
, system-filepath, temporary, text, turtle, unix
}:
mkDerivation {
  pname = "blobstore";
  version = "0.5.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base16-bytestring bytestring exceptions flock foldl
    monad-control optparse-applicative process system-filepath
    temporary text turtle unix
  ];
  executableHaskellDepends = [
    base base16-bytestring bytestring exceptions flock foldl
    monad-control optparse-applicative process system-filepath
    temporary text turtle unix
  ];
  description = "BLOB store on disk";
  license = lib.licenses.bsd3;
}

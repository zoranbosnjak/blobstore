{ mkDerivation, base, base16-bytestring, bytestring, exceptions
, flock, foldl, monad-control, optparse-applicative, process
, stdenv, system-filepath, temporary, text, turtle
}:
mkDerivation {
  pname = "blobstore";
  version = "0.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base16-bytestring bytestring exceptions flock foldl
    monad-control optparse-applicative process system-filepath
    temporary text turtle
  ];
  executableHaskellDepends = [
    base base16-bytestring bytestring exceptions flock foldl
    monad-control optparse-applicative process system-filepath
    temporary text turtle
  ];
  description = "BLOB store on disk";
  license = stdenv.lib.licenses.bsd3;
}

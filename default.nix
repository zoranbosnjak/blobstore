{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, inShell ? null
}:

let

  deps = with pkgs; [
    # package1
    # package2
    # ...
  ];

  haskellPackages = pkgs.haskellPackages.override {
    overrides = haskellPackagesNew: haskellpackagesOld: rec {
      # haskellPackage1 = haskellPackagesNew.callPackage ./nix/myPackage1.nix { };
      # haskellPackage2 = haskellPackagesNew.callPackage ./nix/myPackage2.nix { };
      # ...
    };
  };

  drv1 = haskellPackages.callPackage ./generated.nix { };

  drv = drv1.overrideDerivation (oldAttrs: {
      src = builtins.filterSource
        (path: type:
          (type != "directory" || baseNameOf path != ".git")
          && (type != "symlink" || baseNameOf path != "result"))
        ./.;
    });

  env = pkgs.stdenv.mkDerivation {
    name = drv.env.name;
    buildInputs =
      drv.env.nativeBuildInputs
      ++ deps
      ++ [
        pkgs.haskellPackages.cabal-install
        pkgs.ghcid
        pkgs.cabal2nix
        pkgs.which
      ];
    shellHook = ''
      export LC_ALL=C.UTF-8
      export GHC_BASE=$(which ghc | cut -d '/' -f-4)
    '';
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv


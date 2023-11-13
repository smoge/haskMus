{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, data-default
      , data-default-class, data-default-instances-containers
      , data-default-instances-dlist, data-default-instances-old-locale
      , doctest, fgl, hspec, HUnit, lens, lib, linear, parsec
      , pretty-simple, QuickCheck, random-shuffle, template-haskell
      , test-framework, test-framework-hunit, test-framework-quickcheck2
      , text, vector-space
      }:
      mkDerivation {
        pname = "haskMus";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base containers data-default data-default-class
          data-default-instances-containers data-default-instances-dlist
          data-default-instances-old-locale doctest fgl hspec HUnit lens
          linear parsec pretty-simple QuickCheck random-shuffle
          template-haskell text vector-space
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [
          base hspec HUnit lens QuickCheck test-framework
          test-framework-hunit test-framework-quickcheck2
        ];
        homepage = "https://github.com/smoge/haskMus#readme";
        description = "algorithm composition toolkit in haskell";
        license = lib.licenses.gpl3Plus;
        mainProgram = "haskMus-exe";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

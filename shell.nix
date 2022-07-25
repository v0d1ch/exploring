{ nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/adfcb57e27981eea2c6f3d0cc609119d6186dfaa.tar.gz") {}
, compiler ? "ghc902"
}:


let
  inherit (nixpkgs) pkgs;
  finalPackage = {mkDerivation, hlint, stdenv, zlib, cabal-install}:
    mkDerivation {
      pname = "exploring";
      version = "0.0.1";
      src = ./.;
      isLibrary = true;
      isExecutable = true;
      buildDepends = [zlib];
      libraryToolDepends = [ hlint zlib cabal-install];
      prePatch = "hpack";
      license = stdenv.lib.licenses.bsd3;
      shellHook = ''
       '';
    };

  haskellGhc =
    if compiler == "default"
      then pkgs.haskellGhc
      else pkgs.haskell.packages.${compiler};

  drv = haskellGhc.callPackage finalPackage {};
in
  if pkgs.lib.inNixShell then drv.env else drv

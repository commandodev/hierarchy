with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, containers, foldl, lens, pipes, stdenv, text
             }:
             mkDerivation {
               pname = "hierarchy";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [ base containers foldl lens pipes text ];
               homepage = "https://github.com/boothead/heirarchy";
               license = stdenv.lib.licenses.unfree;
             }) {};
in
  pkg.env

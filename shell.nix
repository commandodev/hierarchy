with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, containers, foldl, free, lens, pipes
             , pipes-bytestring, pipes-parse, profunctors, stdenv, text
             , transformers
             }:
             mkDerivation {
               pname = "hierarchy";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [
                 base containers foldl free lens pipes pipes-bytestring pipes-parse
                 profunctors text transformers
               ];
               homepage = "https://github.com/boothead/heirarchy";
               license = stdenv.lib.licenses.unfree;
             }) {};
in
  pkg.env

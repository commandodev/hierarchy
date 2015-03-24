{ mkDerivation, base, containers, foldl, free, lens, pipes
, pipes-bytestring, pipes-extras, pipes-parse, pipes-csv, pretty-show
, profunctors, stdenv, text, transformers, cassava, QuickCheck
}:
mkDerivation {
  pname = "hierarchy";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base containers foldl free lens pipes pipes-bytestring pipes-extras
    pipes-parse pipes-csv pretty-show profunctors text transformers cassava QuickCheck
  ];
  homepage = "https://github.com/boothead/heirarchy";
  license = stdenv.lib.licenses.unfree;
}

with (import <nixpkgs> {}).pkgs;
let hspkgs = haskell-ng.packages.ghc784.override {
     overrides = self: super: {
       pipes-extras = self.callPackage ../pipes-extras {};
       heirarchy = self.callPackage ./. {};
      };
   };
in
  hspkgs.heirarchy.env

{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", withHoogle ? true}:

let
  inherit (nixpkgs) pkgs;
  packageSet = (
    if compiler == "default"
      then  pkgs.haskellPackages
      else  pkgs.haskell.packages.${compiler}
  );

  haskellPackages = (
    if withHoogle
      then  packageSet.override {
              overrides = (self: super:
                {
                  ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
                  ghcWithPackages = self.ghc.withPackages;
                }
              );
            }
      else  packageSet
  );
    hsPkgs = haskellPackages.override {
      all-cabal-hashes =
        nixpkgs.fetchurl {
          url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/3064cb67fa50443118c22715e6d7a7191415073f.tar.gz";
          sha256 = "0wh271vbay1sb319hzny827yjb87gb2s2pn5i561cbqvnkskg0z8";
        };
      overrides = new: old: {
        sop-core      = new.callHackage "sop-core"     "0.5.0.0" {};
        generics-sop  = new.callHackage "generics-sop" "0.5.0.0" {};
      };
     };

in hsPkgs.callCabal2nix "schemas" ./. {}

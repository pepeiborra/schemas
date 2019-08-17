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
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/b134e7c22f70c7bfef92aaae5fa3bf8868ded6f8.tar.gz";
        sha256 = "111lnz2p75gb7cni324h5cphvig1ian5hg6rxbf167sjnr2sbkjw";
      };
     };

in hsPkgs.callCabal2nix "schemas" ./. {}

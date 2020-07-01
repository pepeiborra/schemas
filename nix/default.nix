let
  pkgs0 = (import ./sources.nix).nixpkgs;
in
{ ghc
}:
rec {
  pkgs = import pkgs0 {};
  haskellPackages = pkgs.haskell.packages.${ghc};
  schemas = haskellPackages.callCabal2nix "schemas" ../. {};
}

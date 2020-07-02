let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};
in
{ ghc
}:
let
  haskellPackages = pkgs.haskell.packages.${ghc};
in
  with pkgs.haskell.lib;
  with haskellPackages;
rec {
  inherit pkgs;
  inherit haskellPackages;
  schemas = callCabal2nix "schemas" ../. {};
  ghcide = dontCheck (callCabal2nix "ghcide" sources.ghcide {
    lsp-test = dontCheck (callHackage "lsp-test" "0.11.0.2" {});
  });
}

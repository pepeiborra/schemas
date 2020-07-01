{ ghc ? "ghc883" }:
let
  nix = import ./nix { inherit ghc; };
in

nix.schemas.env.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ [nix.haskellPackages.ghcide];
})

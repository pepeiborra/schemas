
with (import ./nix {});

{ ghc ? "ghc881"
, haskellPackages ? haskell.packages.${ghc}
}:

haskellPackages.schemas

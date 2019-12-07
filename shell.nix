with (import ./nix {});

{ ghc ? "ghc881"
, haskellPackages ? haskell.packages.${ghc}
}:

let schemasEnv = haskellPackages.schemas.env.overrideAttrs(old: {
      buildInputs = old.buildInputs ++ [haskellPackages.ghcide] ;
    });
in schemasEnv

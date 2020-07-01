{ ghc ? "ghc883" }:
(import ./nix { inherit ghc; }).schemas

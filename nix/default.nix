# inspired by https://github.com/tweag/capability/blob/master/nix/haskell/default.nix
{ nixpkgsSrc ? ./nixpkgs }:
import (import nixpkgsSrc) {
  config = { allowBroken = true; };
  overlays = [ (import ./haskell)];
}

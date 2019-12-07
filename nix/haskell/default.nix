self: super:
{
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc865 = super.haskell.packages.ghc865.override {
          all-cabal-hashes = import ./hackage.nix;
          overrides = hsself: hssuper: {
            schemas = hsself.callCabal2nix "schemas" ../.. {};
            sop-core      = hsself.callHackage "sop-core"     "0.5.0.0" {};
            generics-sop  = hsself.callHackage "generics-sop" "0.5.0.0" {};
          };
        };

      ghc881 = super.haskell.packages.ghc881.override {
          all-cabal-hashes = import ./hackage.nix;
          overrides = hsself: hssuper: {
            schemas = hsself.callCabal2nix "schemas" ../.. {};
              ghcide = (self.haskell.lib.dontCheck (hsself.callHackage "ghcide" "0.0.5" {}));
              haskell-lsp   = self.haskell.lib.doJailbreak (hsself.callHackage "haskell-lsp" "0.18.0.0" {});
              haskell-lsp-types = self.haskell.lib.doJailbreak (hsself.callHackage "haskell-lsp-types" "0.18.0.0" {});
              prettyprinter = hsself.callHackage "prettyprinter" "1.5.1" {};
              hslogger = hsself.callHackage "hslogger" "1.3.1.0" {};
              };
          };

    };
  };
}

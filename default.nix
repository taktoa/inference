{ nixpkgs ? import <nixpkgs> {} }:
let cabal2nixResult = src: nixpkgs.runCommand "cabal2nixResult" {
      buildCommand = ''
        cabal2nix file://"${src}" >"$out"
      '';
      buildInputs = with nixpkgs; [
        cabal2nix
      ];
    } "";
    haskellPackages = nixpkgs.haskellPackages.override {
      overrides = self: super: {
        ava = self.callPackage (cabal2nixResult ./.) {};
        hrecursion-schemes = self.callPackage ./nix/hrecursion-schemes.nix {};
      };
    };
    drv = haskellPackages.ava;
in if nixpkgs.lib.inNixShell then
  drv.env
else
  drv

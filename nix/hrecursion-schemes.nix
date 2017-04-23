{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "hrecursion-schemes";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/mckeankylej/hrecursion-schemes";
    sha256 = "0a4y6z0pgzgpbfd4p4kgcr9lbiwpv70abgw6chzgm04w17lbz7wx";
    rev = "53ab9859d0bb5fb7f5974d0cf04c826764efe867";
  };
  libraryHaskellDepends = [ base ];
  description = "Higher order recursion schemes";
  license = stdenv.lib.licenses.mit;
}

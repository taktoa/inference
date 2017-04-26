{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "hrecursion-schemes";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/mckeankylej/hrecursion-schemes";
    sha256 = "09gqxwf38j090n74alikrs1cv57glvclcn6vvp1hdgqqilcqk4zj";
    rev = "2a14217836f7767ecfcfe30637bf6280bb9be08b";
  };
  libraryHaskellDepends = [ base ];
  description = "Higher order recursion schemes";
  license = stdenv.lib.licenses.mit;
}

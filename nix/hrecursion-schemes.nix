{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "hrecursion-schemes";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/mckeankylej/hrecursion-schemes";
    sha256 = "1amlfc7ykp5gmx70vy9kis3sh0jxn2s8b9bpg8705zqhj47qlsxz";
    rev = "d70176731449f4565f0f8bb690405ef344afdb0c";
  };
  libraryHaskellDepends = [ base ];
  description = "Higher order recursion schemes";
  license = stdenv.lib.licenses.mit;
}

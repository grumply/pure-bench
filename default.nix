{ mkDerivation, base, deepseq, pure-spacetime, pure-test, pure-lifted, pure-json, pure-variance, stdenv }:
mkDerivation {
  pname = "pure-bench";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base deepseq pure-spacetime pure-test pure-lifted pure-json pure-variance ];
  homepage = "github.com/grumply/pure-bench";
  license = stdenv.lib.licenses.bsd3;
}
{ mkDerivation, base, classy-prelude, error-list, hspec
, hspec-expectations, MissingH, mtl, parsec, stdenv
, system-filepath, text, text-render, unordered-containers
}:
mkDerivation {
  pname = "simple-nix";
  version = "0.1.0.3";
  src = ./.;
  buildDepends = [
    base classy-prelude error-list MissingH mtl parsec system-filepath
    text text-render unordered-containers
  ];
  testDepends = [
    base classy-prelude error-list hspec hspec-expectations MissingH
    mtl parsec system-filepath text text-render unordered-containers
  ];
  homepage = "https://github.com/adnelson/simple-nix";
  description = "Simple parsing/pretty printing for Nix expressions";
  license = stdenv.lib.licenses.mit;
}

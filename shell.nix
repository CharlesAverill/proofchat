with import <nixpkgs> {}; stdenv.mkDerivation {
  name = "coq-community-templates";
  buildInputs = [ mustache-go ];
}

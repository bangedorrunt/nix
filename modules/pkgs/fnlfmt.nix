{ lib, pkgs, ... }:
let
  version = "0.2.1";
in
pkgs.stdenv.mkDerivation {
  name = "fnlfmt";
  version = version;
  patchPhase = ''
    patchShebangs ./fennel
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp fnlfmt $out/bin
  '';
  src = pkgs.fetchFromSourcehut {
    owner = "~technomancy";
    repo = "fnlfmt";
    rev = version;
    hash = "sha256-JIqeQhI3fFGrej2wbj6/367IZqWAFegySc2R8IDmvGE=";
  };
  buildInputs = lib.attrVals [ "lua" ] pkgs;
}

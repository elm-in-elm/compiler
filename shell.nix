{ pkgs ? import <nixpkgs> {} }:
with pkgs;
with stdenv;
mkShell {
  buildInputs = with elmPackages; [ 
    elm elm-format
    nodejs-14_x 
  ];
}

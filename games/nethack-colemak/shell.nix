{ pkgs ? import <nixpkgs> {} }:

let
  nethack = pkgs.nethack.overrideAttrs(attr: {
    patches = [./keys.patch];
  });
in pkgs.stdenv.mkDerivation {
  name = "nethack-colemak";
  buildInputs = [
    nethack
  ];
}

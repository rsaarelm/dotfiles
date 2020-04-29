{ pkgs, ... }:

let
  nethack-colemak = pkgs.nethack.overrideAttrs(attr: {
    patches = [./nethack-colemak.patch];
  });
in
{
  home.packages = [
    nethack-colemak
  ];
}

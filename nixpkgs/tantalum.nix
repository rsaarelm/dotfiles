{ pkgs, ... }:

{
  imports = [
    ./common.nix
    ./autorandr/tantalum.nix

    ./style/dark-theme.nix
  ];

}

{ pkgs, ... }:

{
  imports = [
    ./common.nix
    ./autorandr/tantalum.nix

    ./style/dark-theme.nix
  ];

  home.packages = with pkgs; [
    steam
  ];
}

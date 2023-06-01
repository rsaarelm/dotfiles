{ pkgs, ... }:

{
  imports = [
    ./gui-core.nix
    ./extras.nix
    ./programs/chromium.nix
    ./programs/zathura.nix

    ./style/dark-theme.nix
  ];
}

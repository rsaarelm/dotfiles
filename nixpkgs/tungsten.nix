{ pkgs, ... }:

{
  imports = [
    ./common.nix
    ./autorandr/tungsten.nix

    ./style/dark-theme.nix
  ];

  home.packages = with pkgs; [
    steam
  ];

  xsession.profileExtra = ''
    autorandr lowres
  '';
}

{ pkgs, ... }:

{
  xdg.configFile."i3/status.toml".text = ''
    theme = "plain"
    icons = "awesome"

    [[block]]
    block = "net"
    device = "enp3s0"
    format = "{ip}"
  '';

  imports = [
    ./common.nix
    ./packages.nix
    ./latex.nix
    ./autorandr/tungsten.nix

    ./style/light-theme.nix
  ];

  home.packages = with pkgs; [
    steam
    steam-run-native
  ];

  xsession.profileExtra = ''
    autorandr lowres
  '';
}

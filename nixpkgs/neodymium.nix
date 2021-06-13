{ pkgs, ... }:

{
  xdg.configFile."i3/status.toml".text = ''
    theme = "plain"
    icons = "awesome"

    [[block]]
    block = "temperature"
    collapsed = false

    [[block]]
    block = "net"
    device = "wlp1s0"
    format = "{ip}"
  '';

  imports = [
    ./common.nix
    ./packages.nix

    ./style/dark-theme.nix
  ];

  home.packages = with pkgs; [
  ];

  xsession.profileExtra = ''
  '';
}

{ pkgs, ... }:

{
  imports = [
    ./gui-core.nix
    ./extras.nix
    ./programs/chromium.nix
    ./programs/zathura.nix

    ./style/dark-theme.nix
  ];

  xdg.configFile."i3/status.toml".text = ''
    theme = "plain"
    icons = "awesome"

    [[block]]
    block = "temperature"
    collapsed = false

    [[block]]
    block = "net"
    device = "wlan0"
    format = "{ip}"
  '';
}

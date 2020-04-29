{ pkgs, ... }:

{
  xdg.configFile."i3/status.toml".text = ''
    theme = "plain"
    icons = "awesome"

    [[block]]
    block = "bluetooth"
    mac = "EB:06:EF:74:4B:47"

    [[block]]
    block = "battery"

    [[block]]
    block = "net"
    device = "wlp1s0"
    ip = true
    speed_up = false
    speed_down = false

    ssid = true
    signal_strength = true
  '';

  imports = [
    ./common.nix
    ./autorandr/tantalum.nix

    ./style/dark-theme.nix
  ];

  home.packages = with pkgs; [
    steam
  ];
}

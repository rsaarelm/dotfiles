{ pkgs, ... }:

{
  xdg.configFile."i3/status.toml".text = ''
    theme = "plain"
    icons = "awesome"

    [[block]]
    block = "net"
    device = "enp3s0"
    ip = true
    speed_up = false
    speed_down = false
  '';

  imports = [
    ./common.nix
    ./autorandr/tungsten.nix

    ./style/dark-theme.nix
  ];

  home.packages = with pkgs; [
    steam
    steam-run-native
  ];

  xsession.profileExtra = ''
    autorandr lowres
  '';
}

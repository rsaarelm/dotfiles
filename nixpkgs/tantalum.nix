{ pkgs, ... }:

let
  adom = pkgs.callPackage ./pkgs/adom { };
in
{
  imports = [
    ./gui-core.nix
    ./extras.nix
    ./programs/chromium.nix
    ./programs/texlive.nix
    ./programs/zathura.nix

    ./style/light-theme.nix
  ];

  home.packages = with pkgs; [
    steam
    steam-run-native
    adom
  ];

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
    format = "{ssid}"
  '';

  xdg.configFile."nvim/guifont.vim".text = ''
    Guifont Source\ Code\ Pro:h9
  '';
}

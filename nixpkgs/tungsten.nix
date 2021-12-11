{ pkgs, ... }:

{
  imports = [
    ./gui-core.nix
    ./extras.nix
    ./programs/chromium.nix
    ./programs/texlive.nix
    ./programs/zathura.nix

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

  xdg.configFile."i3/status.toml".text = ''
    theme = "plain"
    icons = "awesome"

    [[block]]
    block = "net"
    device = "enp3s0"
    format = "{ip}"
  '';

  xdg.configFile."nvim/guifont.vim".text = ''
    Guifont Source\ Code\ Pro:h11
  '';
}

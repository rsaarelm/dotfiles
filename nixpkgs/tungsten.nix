{ pkgs, ... }:

{
  imports = [
    ./gui-core.nix
    ./extras.nix
    ./programs/chromium.nix
    ./programs/texlive.nix
    ./programs/zathura.nix

    ./style/dark-theme.nix
  ];

  home.packages = with pkgs; [
    wineWowPackages.stable
    steam
    steam-run-native
  ];

  # Tungsten's multi-monitor setup
  wayland.windowManager.sway.config = {
    output = {
      DP-1 = {
        pos = "0 0";
        scale = "2";
      };
      DVI-I-1 = {
        # NB: Use DPI-scaled coordinates for position
        pos = "1920 0";
        transform = "270";
      };
    };

    # Left-hand keys, left monitor; right-hand keys right monitor
    workspaceOutputAssign = [
      { workspace = "1";  output = "DP-1"; }
      { workspace = "2";  output = "DP-1"; }
      { workspace = "3";  output = "DP-1"; }
      { workspace = "4";  output = "DP-1"; }
      { workspace = "5";  output = "DP-1"; }
      { workspace = "6";  output = "DVI-I-1"; }
      { workspace = "7";  output = "DVI-I-1"; }
      { workspace = "8";  output = "DVI-I-1"; }
      { workspace = "9";  output = "DVI-I-1"; }
      { workspace = "10"; output = "DVI-I-1"; }
    ];
  };

  xsession.pointerCursor = {
    name = "Adwaita";
    package = pkgs.gnome.adwaita-icon-theme;
    size = 32;
  };

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

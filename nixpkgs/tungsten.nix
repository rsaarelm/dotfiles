{ pkgs, ... }:

{
  imports = [
    ./gui-core.nix
    ./extras.nix
    ./programs/chromium.nix
    ./programs/texlive.nix
    ./programs/zathura.nix

    ./autorandr/tungsten.nix
    ./style/dark-theme.nix
  ];

  home.packages = with pkgs; [
    wineWowPackages.stable
    steam
    steam-run-native
  ];

  home.pointerCursor = {
    name = "Adwaita";
    package = pkgs.gnome.adwaita-icon-theme;
    size = 32;
  };

  xdg.configFile."nvim/guifont.vim".text = ''
    Guifont Source\ Code\ Pro:h11
  '';
}

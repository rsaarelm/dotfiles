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

    ./autorandr/tantalum.nix
    ./style/dark-theme.nix
  ];

  home.packages = with pkgs; [
    steam
    steam-run-native
    adom
  ];

  xdg.configFile."nvim/guifont.vim".text = ''
    Guifont Source\ Code\ Pro:h9
  '';
}

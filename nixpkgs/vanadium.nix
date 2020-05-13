{ pkgs, ... }:

# Debian box with userspace nix, some tweaking is needed.

{
  imports = [
    ./common.nix

    ./style/light-theme.nix
  ];

  home.packages = with pkgs; [
    glibcLocales
  ];

  # Fix for home-managed rofi on Debian, https://github.com/rycee/home-manager/issues/354
  home.sessionVariables.LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";

  # Make nix-installed programs find terminfo
  # https://github.com/rycee/home-manager/issues/706
  home.sessionVariables.TERMINFO_DIRS = "/lib/terminfo";
}

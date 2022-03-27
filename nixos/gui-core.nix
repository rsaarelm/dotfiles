# Minimal GUI environment.
{ pkgs, ... }:

{
  # Expands on cli-core, so that gets imported too.
  imports = [
    ./cli-core.nix
  ];

  fonts.fonts = with pkgs; [
    terminus_font
    tamzen
    source-code-pro
    open-dyslexic

    # Fonts with extra symbols
    noto-fonts-cjk
    font-awesome_5
    material-design-icons
    powerline-fonts
  ];

  # GUI-only zsh aliases: Switch between qwerty and colemak
  programs.zsh = {
    shellAliases = {
      arst = "setxkbmap us -variant intl";
      asdf = "setxkbmap us -variant colemak";
    };
  };

  # Hide immobile mouse cursor.
  services.unclutter-xfixes.enable = true;

  # Sound on
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;

    # Bluetooth support
    package = pkgs.pulseaudioFull;
  };
}

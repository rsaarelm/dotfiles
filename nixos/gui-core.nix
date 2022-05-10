# Minimal GUI environment.
{ pkgs, ... }:

{
  # Expands on cli-core, so that gets imported too.
  imports = [
    ./cli-core.nix
  ];

  services.xserver = {
    enable = true;

    displayManager = {
      defaultSession = "none+i3";

      autoLogin.enable = true;
      autoLogin.user = "rsaarelm";

      lightdm.enable = true;
    };

    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        i3lock
        i3status-rust
        libnotify
        pavucontrol
        rofi
        rxvt_unicode-with-plugins
      ];
    };

    layout = "us";
    xkbVariant = "colemak";
    xkbOptions = "ctrl:nocaps";

    desktopManager.xterm.enable = false;

    # Lock screen automatically.
    xautolock = {
      enable = true;
      time = 60; # minutes
      locker = "${pkgs.i3lock}/bin/i3lock --color 002222";
      notify = 20; # seconds
      notifier =
        "${pkgs.libnotify}/bin/notify-send 'Locking machine in 20 seconds'";
      extraOptions = [ "-detectsleep" ];
    };
  };

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

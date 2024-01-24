{ pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs;
      let neovim = pkgs.neovim.override { vimAlias = true; };
      in [
        # System
        acpi
        acpitool
        at
        bluez
        cachix
        coreutils
        exfat
        inotifyTools
        jmtpfs
        lm_sensors
        openvpn
        pciutils
        psmisc
        usbutils

        # Basic tools
        direnv
        fd
        file
        fzf
        git
        git-annex
        just
        links2
        mosh
        neovim
        p7zip
        pwgen
        ripgrep
        tmux
        wget
      ];

      variables = {
        EDITOR = pkgs.lib.mkOverride 0 "nvim";
        TERMINAL = "alacritty";
      };
  };

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
        alacritty
        i3lock
        i3status-rust
        libnotify
        pavucontrol
        rofi

        chromium
        neovim-qt
        sxiv
        zathura
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
    intel-one-mono
    source-code-pro

    # Fonts with extra symbols
    noto-fonts-cjk
    font-awesome
    powerline-fonts
  ];

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

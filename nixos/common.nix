{ config, pkgs, ... }:

{
  boot.kernelModules = [ "snd-seq" "snd-rawmidi" ];

  boot.cleanTmpDir = true;

  # LOCALIZATION

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "colemak/en-latin9";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Helsinki";

  # PACKAGES

  security.sudo.wheelNeedsPassword = false;

  environment = {
    systemPackages = with pkgs;
    let
      neovim = pkgs.neovim.override { vimAlias = true; };
    in
    [
      # System
      acpi
      acpitool
      bluez
      coreutils
      file
      jmtpfs
      lsof
      pciutils
      psmisc
      sshfs
      usbutils

      # Utils
      aspell
      aspellDicts.en
      cmus
      fdupes
      gitAndTools.git-annex
      hledger
      imagemagick
      links
      mosh
      ncdu
      neovim
      optipng
      p7zip
      pandoc
      pwgen
      ranger
      restic
      stow
      thefuck
      tmux
      unzip
      wget
      xxd
      youtubeDL

      # Dev
      binutils
      clang
      git
      gnumake
      python3

      # X11
      autorandr
      chromium
      gimp
      grafx2
      keepass
      mpv
      neovim-qt
      pavucontrol
      rofi
      rxvt_unicode_with-plugins
      scrot
      sxiv
      xcape
      xorg.xmessage
      xorg.xrefresh
      zathura
    ];

    variables.EDITOR = pkgs.lib.mkOverride 0 "nvim";
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  programs = {
    ssh.startAgent = true;

    zsh = {
      enable = true;
      enableCompletion = true;
    };
  };

  # List services that you want to enable:
  services = {
    openssh = {
      enable = true;

      # No password guessing
      passwordAuthentication = false;
      challengeResponseAuthentication = false;
    };

    locate.enable = true;
    printing.enable = true;

    redshift = {
      enable = true;
      latitude = "25";
      longitude = "60";
      temperature.day = 6500;
      temperature.night = 3500;
    };

    xserver = {
      enable = true;
      windowManager = {
        default = "i3";
        i3.enable = true;
      };

      layout = "us(colemak)";
      xkbOptions = "ctrl:nocaps";

      displayManager.slim.defaultUser = "rsaarelm";

      displayManager.sessionCommands = ''
        xrdb "${pkgs.writeText "xrdb.conf" ''
          URxvt.font: xft:Dina:style=bold
          URxvt.scrollBar: false
          URxvt.perl-ext: default,url-select
          URxvt.keysym.M-u: perl:url-select:select_next
          URxvt.url-select.launcher: chromium --incognito
          URxvt.url-select.underline: true
          URxvt.saveLines: 32000

          *color0: #2e3436
          *color1: #cc0000
          *color2: #4e9a06
          *color3: #c4a000
          *color4: #3465a4
          *color5: #ff00e4
          *color6: #00fbff
          *color7: #d3d7cf
          *color8: #565654
          *color9: #ee3030
          *color10: #8ae234
          *color11: #fce94f
          *color12: #729fcf
          *color13: #b292af
          *color14: #a2ffff
          *color15: #eeeeec

          *background: Gray10
          *foreground: Gray
        ''}"
      '';
    };

    unclutter-xfixes.enable = true;
  };

  systemd.user.services.xcape = {
    enable = true;
    description = "xcape to use CTRL as ESC when pressed alone";
    wantedBy = [ "default.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.xcape}/bin/xcape";
  };

  fonts.fonts = with pkgs; [
    dina-font-pcf
    noto-fonts
    noto-fonts-emoji
  ];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;

    # Bluetooth support
    package = pkgs.pulseaudioFull;
  };

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.rsaarelm = {
    isNormalUser = true;
    home = "/home/rsaarelm";
    extraGroups = [ "wheel" "audio" ];
    uid = 1000;
    shell = pkgs.zsh;
    initialPassword = "1234";
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQ2yWs8ZtckDHk/e/ryrnfLbOTLP5C4ngGnnDP49d4PLJmXoUlHTi2ZMrGGISdyQsklqxeMIxpN2JWF/OsliiDzqwd8PiqvTHWcBIXOqqQRnRAXsiVbHcyx/iD/c2kxxfK4Hg6yhOZTzQe93agMeAQPILx20W7Y1vXtn9lQhJ2RuA9Zf5XVVbtVFvdRDIRDrmNM3nZMyko+C5E75Y5i+JJOJ0ORnl3fkimv/k8PmzU9W9gvzjL1aQqEpLjsTEPyD7/LdKSLQIyqYTWrZuVLZ2ROzO2ftsDswpIgTuyavWKAOON/HU11SBAbwalbPp9Q5VJDLfKe6fCE5iYgOtXlKmB" ];
  };

  # Cleanup the nix store
  nix.gc.automatic = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}

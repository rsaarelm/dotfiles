{ config, pkgs, ... }:

{
  boot.kernelModules = [ "snd-seq" "snd-rawmidi" ];

  boot.cleanTmpDir = true;

  # LOCALIZATION

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "colemak/colemak";
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
      docker
      exfat-utils
      file
      fuse_exfat
      inotifyTools
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
      htop
      imagemagick
      k2pdfopt
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
      tree
      unzip
      wget
      xxd
      youtubeDL

      # LaTeX
      (texlive.combine {
        inherit (texlive)
          collection-basic
          metafont
          xits
          collection-bibtexextra
          collection-binextra
          collection-context
          collection-formatsextra
          collection-fontutils
          collection-langenglish
          collection-latex
          collection-latexextra
          collection-latexrecommended
          collection-pictures
          collection-pstricks
          collection-xetex;
      })

      # Dev
      binutils
      clang
      git
      gnumake
      python3

      # X11
      # Stable Anki is bitrotted, use local install for now
      #anki
      autorandr
      chromium
      gimp
      grafx2
      keepass
      libnotify
      mpv
      neovim-qt
      nitrogen
      notify-osd
      pavucontrol
      rofi
      rxvt_unicode-with-plugins
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

  virtualisation.docker.enable = true;

  location.latitude = 25.;
  location.longitude = 60.;

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
    earlyoom.enable = true;

    redshift = {
      enable = true;
      temperature.day = 6500;
      temperature.night = 3500;

      brightness.day = "1";
      brightness.night = "0.5";
    };

    xserver = {
      enable = true;
      windowManager = {
        default = "i3";
        i3.enable = true;
      };

      layout = "us(colemak)";
      xkbOptions = "ctrl:nocaps";

      desktopManager.xterm.enable = false;

      xautolock = {
        enable = true;
        time = 10;         # minutes
        locker = "${pkgs.i3lock}/bin/i3lock --color 002222";
        notify = 10;       # seconds
        notifier = "${pkgs.libnotify}/bin/notify-send 'Locking machine in 10 seconds'";
        extraOptions = [ "-secure -detectsleep" ];
      };

      displayManager.sessionCommands = ''
        xrdb "${pkgs.writeText "xrdb.conf" ''
          URxvt.font:                 xft:Go Mono:size=12,xft:mononoki,xft:Noto Sans Mono CJK JP,xft:Symbola
          URxvt.scrollBar:            false
          URxvt.saveLines:            32000
          URxvt.perl-ext-common:      default,url-select,resize-font
          URxvt.keysym.M-u:           perl:url-select:select_next
          URxvt.url-select.launcher:  chromium --incognito
          URxvt.url-select.underline: true
          URxvt.resize-font.step:     2
          URxvt.keysym.C-equal:       font-size:increase
          URxvt.keysym.C-minus:       font-size:decrease


          ! special
          *.foreground:   #262626
          *.background:   #e0e0e0
          ! black
          *.color0:       #1b1b1b
          *.color8:       #6d6d6d
          ! red
          *.color1:       #7d1e1e
          *.color9:       #ee3030
          ! green
          *.color2:       #356C00
          *.color10:      #61be07
          ! yellow
          *.color3:       #7f702d
          *.color11:      #beb03c
          ! blue
          *.color4:       #254d80
          *.color12:      #3f7ec1
          ! magenta
          *.color5:       #802576
          *.color13:      #c546ba
          ! cyan
          *.color6:       #00787a
          *.color14:      #76baba
          ! white
          *.color7:       #c2c2c2
          *.color15:      #ffffff
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
    gohufont
    terminus_font

    go-font
    hack-font
    mononoki
    symbola
    noto-fonts-cjk
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
    extraGroups = [ "wheel" "audio" "docker" ];
    uid = 1000;
    shell = pkgs.zsh;
    initialPassword = "1234";
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQ2yWs8ZtckDHk/e/ryrnfLbOTLP5C4ngGnnDP49d4PLJmXoUlHTi2ZMrGGISdyQsklqxeMIxpN2JWF/OsliiDzqwd8PiqvTHWcBIXOqqQRnRAXsiVbHcyx/iD/c2kxxfK4Hg6yhOZTzQe93agMeAQPILx20W7Y1vXtn9lQhJ2RuA9Zf5XVVbtVFvdRDIRDrmNM3nZMyko+C5E75Y5i+JJOJ0ORnl3fkimv/k8PmzU9W9gvzjL1aQqEpLjsTEPyD7/LdKSLQIyqYTWrZuVLZ2ROzO2ftsDswpIgTuyavWKAOON/HU11SBAbwalbPp9Q5VJDLfKe6fCE5iYgOtXlKmB" ];
  };

  # Cleanup the nix store
  nix.gc.automatic = true;
}

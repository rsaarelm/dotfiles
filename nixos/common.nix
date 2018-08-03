{ config, pkgs, ... }:

{
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
      coreutils
      file
      lsof
      psmisc
      sshfs

      # Utils
      aspell
      aspellDicts.en
      gitAndTools.git-annex
      hledger
      imagemagick
      links
      mosh
      ncdu
      neovim
      p7zip
      pandoc
      pwgen
      restic
      stow
      tmux
      unzip
      wget

      # Dev
      binutils
      clang
      git
      rustup

      # X11
      chromium
      gimp
      grafx2
      mpv
      neovim-qt
      pavucontrol
      rofi
      rxvt_unicode_with-plugins
      scrot
      sxiv
      zathura
    ];

    variables.EDITOR = pkgs.lib.mkOverride 0 "nvim";
  };

  nixpkgs.config = {
    allowUnfree = true;

    # TODO: Can this be removed?
    rxvt_unicode = {
      perlSupport = true;
    };
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

      displayManager.slim.defaultUser = "rsaarelm";

      displayManager.sessionCommands = ''
        xrdb "${pkgs.writeText "xrdb.conf" ''
          URxvt.font: xft:Monospace:size=12
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
  };

  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    liberation_ttf
    dina-font
    proggyfonts
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
  hardware.pulseaudio.enable = true;

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.rsaarelm = {
    isNormalUser = true;
    home = "/home/rsaarelm";
    extraGroups = [ "wheel" ];
    uid = 1000;
    shell = pkgs.zsh;
    initialPassword = "1234";
  };

  # Cleanup the nix store
  nix.gc.automatic = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # BOOT

  # Use GRUB to boot
  boot.loader.grub.device = "/dev/sda";

  # NETWORK

  networking.hostName = "tungsten"; # Define your hostname.

  # LOCALIZATION

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "colemak/en-latin9";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Helsinki";

  # HARDWARE

  # Steam games want this.
  hardware = {
    opengl.driSupport32Bit = true;
    pulseaudio.support32Bit = true;
  };

  # PACKAGES

  security.sudo.wheelNeedsPassword = false;

  environment = {
    systemPackages = with pkgs;
    let
      neovim = pkgs.neovim.override { vimAlias = true; };
    in
    [
      # Terminal
      coreutils
      git
      gitAndTools.git-annex
      hledger
      imagemagick
      links
      mosh
      ncdu
      neovim
      pandoc
      psmisc
      sshfs
      stow
      tmux
      wget

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
      unclutter
      zathura

      discord
    ];

    variables.EDITOR = pkgs.lib.mkOverride 0 "nvim";
  };

  nixpkgs.config = {
    allowUnfree = true;

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
    openssh.enable = true;
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

  # Tungsten's dual monitor setup
  services.xserver.xrandrHeads = [
    {
      output = "DP-1";
      primary = true;
      monitorConfig = ''
        Option "PreferredMode" "1920x1080"
      '';
    }
    {
      output = "DVI-I-1";
      monitorConfig = ''
        Option "PreferredMode" "1920x1200"
        Option "Rotate" "left"
      '';
    }
  ];

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
  };

  # SYSTEMD

  systemd.user.services = {
    "unclutter" = {
      enable = true;
      description = "Hide unmoving mouse cursor";
      wantedBy = [ "default.target" ];
      serviceConfig.Restart = "always";
      serviceConfig.RestartSec = 2;
      serviceConfig.ExecStart = "${pkgs.unclutter}/bin/unclutter";
    };
  };


  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}

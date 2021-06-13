{ config, pkgs, ... }:

{
  # Import the non-version-controlled wifi config file if present.
  imports = [ ./home-network.nix ]
    ++ (if builtins.pathExists ./wifi.nix then [ ./wifi.nix ] else [ ]);

  boot.kernelModules = [ "snd-seq" "snd-rawmidi" ];

  boot.cleanTmpDir = true;

  # Link identical store files together.
  nix.autoOptimiseStore = true;

  # LOCALIZATION

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    font = "Lat2-Terminus16";
    keyMap = "colemak/colemak";
  };

  # Set your time zone.
  time.timeZone = "Europe/Helsinki";

  # PACKAGES

  security.sudo.wheelNeedsPassword = false;

  environment = {
    systemPackages = with pkgs;
      let neovim = pkgs.neovim.override { vimAlias = true; };
      in [
        # System
        acpi
        acpitool
        bluez
        coreutils
        exfat-utils
        fuse_exfat
        inotifyTools
        lm_sensors
        openvpn
        pciutils
        psmisc
        usbutils

        # Basic tools
        direnv
        file
        git
        home-manager
        htop
        jq
        just
        links
        lsof
        mosh
        neovim
        p7zip
        pwgen
        sshfs
        tmux
        unzip
        wget
        xxd

        # X11
        i3lock
        i3status-rust
        libnotify
        pavucontrol
        rofi
        rxvt_unicode-with-plugins
      ];

    variables.EDITOR = pkgs.lib.mkOverride 0 "nvim";
  };

  nixpkgs.config = { allowUnfree = true; };

  programs = {
    ssh.startAgent = true;

    zsh = {
      enable = true;
      enableCompletion = true;
    };
  };

  virtualisation.docker.enable = true;

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

    xserver = {
      enable = true;

      displayManager = {
        defaultSession = "none+i3";

        autoLogin.enable = true;
        autoLogin.user = "rsaarelm";

        lightdm.enable = true;
      };

      windowManager.i3.enable = true;

      layout = "us(colemak)";
      xkbOptions = "ctrl:nocaps";

      desktopManager.xterm.enable = false;

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

    unclutter-xfixes.enable = true;
  };

  fonts.fonts = with pkgs; [
    # Bitmap
    gohufont
    terminus_font

    # TTF
    go-font
    hack-font
    mononoki
    symbola
    noto-fonts-cjk
    font-awesome_4
    source-code-pro
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
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQ2yWs8ZtckDHk/e/ryrnfLbOTLP5C4ngGnnDP49d4PLJmXoUlHTi2ZMrGGISdyQsklqxeMIxpN2JWF/OsliiDzqwd8PiqvTHWcBIXOqqQRnRAXsiVbHcyx/iD/c2kxxfK4Hg6yhOZTzQe93agMeAQPILx20W7Y1vXtn9lQhJ2RuA9Zf5XVVbtVFvdRDIRDrmNM3nZMyko+C5E75Y5i+JJOJ0ORnl3fkimv/k8PmzU9W9gvzjL1aQqEpLjsTEPyD7/LdKSLQIyqYTWrZuVLZ2ROzO2ftsDswpIgTuyavWKAOON/HU11SBAbwalbPp9Q5VJDLfKe6fCE5iYgOtXlKmB"
    ];
  };

  # Cleanup the nix store
  nix.gc.automatic = true;
}

# System settings, should be same on every box.
# No software installs or hardware-specific stuff here.
# This needs to be usable by very minimal builds.
{ config, pkgs, ... }:

{
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

  # SYSTEM CONFIG

  security.sudo.wheelNeedsPassword = false;

  services.earlyoom.enable = true;

  # Maybe keeps SSD drives healthier.
  services.fstrim.enable = true;

  # Maintain file locate db.
  services.locate.enable = true;

  # Automatically garbage collect after a while.
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = " --delete-older-than 30d";
  };

  # Flakes
  nix.package = pkgs.nixFlakes;

  nix.extraOptions = ''
    # Smart garbage collection when space is getting tight.
    # "Free up to 1GiB whenever there is less than 100MiB left."
    min-free = ${toString (100 * 1024 * 1024)}
    max-free = ${toString (1024 * 1024 * 1024)}

    experimental-features = nix-command flakes
  '';

  # SHELL

  programs = {
    ssh.startAgent = true;

    zsh = {
      enable = true;
      enableCompletion = true;
      histSize = 10000;
      setOptions = [
        "HIST_IGNORE_DUPS"
        "SHARE_HISTORY"
        "HIST_FCNTL_LOCK"
        "EXTENDED_HISTORY"
        "HIST_IGNORE_SPACE"
        "HIST_EXPIRE_DUPS_FIRST"
        "autocd"
      ];

      shellAliases = {
        # Start week on Monday even on US locale.
        cal = "cal -m";
      };

      promptInit = ''
        #       [- path    -][- error code -]
        PROMPT='%F{green}%~%f%(?.. %F{red}%?%f) %F{green}%%%f '

        #        [- username   -] [- clock            -]
        RPROMPT='%F{green}%n@%m%f %F{cyan}%D{%H:%M:%S}%f'
      '';
    };
  };

  services = {
    openssh = {
      enable = true;

      # No password guessing
      passwordAuthentication = false;
      challengeResponseAuthentication = false;
    };
  };

  # USER SETUP

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.rsaarelm = {
    isNormalUser = true;
    home = "/home/rsaarelm";
    extraGroups = [ "wheel" "audio" "docker" "vboxusers" ];
    uid = 1000;
    shell = pkgs.zsh;
    initialPassword = "1234";
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQ2yWs8ZtckDHk/e/ryrnfLbOTLP5C4ngGnnDP49d4PLJmXoUlHTi2ZMrGGISdyQsklqxeMIxpN2JWF/OsliiDzqwd8PiqvTHWcBIXOqqQRnRAXsiVbHcyx/iD/c2kxxfK4Hg6yhOZTzQe93agMeAQPILx20W7Y1vXtn9lQhJ2RuA9Zf5XVVbtVFvdRDIRDrmNM3nZMyko+C5E75Y5i+JJOJ0ORnl3fkimv/k8PmzU9W9gvzjL1aQqEpLjsTEPyD7/LdKSLQIyqYTWrZuVLZ2ROzO2ftsDswpIgTuyavWKAOON/HU11SBAbwalbPp9Q5VJDLfKe6fCE5iYgOtXlKmB"
    ];
  };
}

{ config, pkgs, ... }:

{
  boot.tmp.cleanOnBoot = true;

  # Link identical store files together.
  nix.settings.auto-optimise-store = true;

  nixpkgs.config.allowUnfree = true;

  # LOCALIZATION

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_TIME = "en_DK.UTF-8";
    };
  };

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

  # At daemon
  services.atd.enable = true;

  location = {
    provider = "manual";
    latitude = 60.0;
    longitude = 25.0;
  };

  services.redshift = {
    enable = true;
    brightness = {
      day = "1";
      night = "1";
    };
    temperature = {
      day = 5500;
      night = 3700;
    };
  };

  # Automatically garbage collect after a while.
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = " --delete-older-than 30d";
  };

  nix.extraOptions = ''
    # Smart garbage collection when space is getting tight.
    # "Free up to 1GiB whenever there is less than 100MiB left."
    min-free = ${toString (100 * 1024 * 1024)}
    max-free = ${toString (1024 * 1024 * 1024)}

    experimental-features = nix-command flakes
  '';

  # Caps is ctrl
  services.interception-tools = {
    enable = true;
    plugins = [ pkgs.interception-tools-plugins.caps2esc ];
    udevmonConfig = ''
      - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools-plugins.caps2esc}/bin/caps2esc -m 1 | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE"
        DEVICE:
          EVENTS:
            EV_KEY: [KEY_CAPSLOCK, KEY_ESC]
    '';
  };

  # SHELL

  programs = {
    ssh.startAgent = true;

    fish = {
      enable = true;
    };
  };

  services = {
    openssh = {
      enable = true;

      # No password guessing
      settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
      };
    };
  };

  # USER SETUP

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.rsaarelm = {
    isNormalUser = true;
    home = "/home/rsaarelm";
    extraGroups = [ "wheel" "audio" "docker" "vboxusers" ];
    uid = 1000;
    shell = pkgs.fish;
    initialPassword = "1234";
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQ2yWs8ZtckDHk/e/ryrnfLbOTLP5C4ngGnnDP49d4PLJmXoUlHTi2ZMrGGISdyQsklqxeMIxpN2JWF/OsliiDzqwd8PiqvTHWcBIXOqqQRnRAXsiVbHcyx/iD/c2kxxfK4Hg6yhOZTzQe93agMeAQPILx20W7Y1vXtn9lQhJ2RuA9Zf5XVVbtVFvdRDIRDrmNM3nZMyko+C5E75Y5i+JJOJ0ORnl3fkimv/k8PmzU9W9gvzjL1aQqEpLjsTEPyD7/LdKSLQIyqYTWrZuVLZ2ROzO2ftsDswpIgTuyavWKAOON/HU11SBAbwalbPp9Q5VJDLfKe6fCE5iYgOtXlKmB"
    ];
  };

  nix.settings.trusted-users = [ "root" "@wheel" ];
}

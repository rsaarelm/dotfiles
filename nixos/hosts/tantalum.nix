{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./common.nix
      ./wifi.nix
      ./home-network.nix
      ./hardware-configuration.nix
    ];

  # BOOT

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # NETWORK

  networking.hostName = "tantalum";

  # HARDWARE

  hardware = {
    # Steam games want this.
    opengl.driSupport32Bit = true;

    bluetooth = {
      enable = true;
      extraConfig = ''
        [General]
        Enable=Source,Sink,Media,Socket
      '';
    };
  };

  # SERVICES

  services = {
    xserver = {
      # Touchpad
      libinput = {
        enable = true;
        disableWhileTyping = true;
      };
    };

    autorandr.enable = true;

    acpid = {
      enable = true;

      # Disable touchpad when lid is closed, otherwise it randomly emits mouse noise.
      lidEventCommands = ''
        LID_STATE=$(/run/current-system/sw/bin/awk '{print $2}' /proc/acpi/button/lid/LID/state)
        case $LID_STATE in
          closed)
            DISPLAY=:0.0 XAUTHORITY=/home/rsaarelm/.Xauthority /run/current-system/sw/bin/xinput --disable "Elan Touchpad";;
          open)
            DISPLAY=:0.0 XAUTHORITY=/home/rsaarelm/.Xauthority /run/current-system/sw/bin/xinput --enable "Elan Touchpad";;
        esac
      '';
    };
  };

  services.xserver.dpi = 120;

  # PACKAGES

  environment.systemPackages = with pkgs; [
    discord
    steam
  ];

  system.stateVersion = "18.03";
}

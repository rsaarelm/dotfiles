{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../common.nix
    ../../apps.nix
    ../../home-network.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # NETWORK

  networking.hostName = "tantalum";
  networking.networkmanager.enable = true;

  hardware = {
    # Steam games want this.
    graphics.enable32Bit = true;

    bluetooth = {
      enable = true;
      settings = { General.Enable = "Source,Sink,Media,Socket"; };
    };
  };

  environment.systemPackages = with pkgs; [
    steam
    discord
  ];

  services = {
    # Touchpad
    libinput = {
      enable = true;
      touchpad.disableWhileTyping = true;
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

  services.xserver.dpi = 144;

  system.stateVersion = "18.03";
}

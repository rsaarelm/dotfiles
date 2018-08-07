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

      displayManager.slim.theme = ./slim-theme;
    };

    autorandr.enable = true;
  };


  # PACKAGES

  environment.systemPackages = with pkgs; [
    autorandr

    discord
    steam
  ];
}

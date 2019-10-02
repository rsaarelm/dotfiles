{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./common.nix
      ./home-network.nix
      ./hardware-configuration.nix
    ];

  # BOOT

  # Use GRUB to boot
  boot.loader.grub.device = "/dev/sda";

  # NETWORK

  networking.hostName = "tungsten";

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

  services.xserver.videoDrivers = ["nvidiaBeta"];

  # Dual monitor setup
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

  system.stateVersion = "18.03";
}

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./common.nix
      ./hardware-configuration.nix
    ];

  # BOOT

  # Use GRUB to boot
  boot.loader.grub.device = "/dev/sda";

  # NETWORK

  networking.hostName = "tungsten"; # Define your hostname.

  # HARDWARE

  # Steam games want this.
  hardware = {
    opengl.driSupport32Bit = true;
    pulseaudio.support32Bit = true;
  };

  # PACKAGES

  environment.systemPackages = with pkgs; [
    discord
    steam
  ];

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
}

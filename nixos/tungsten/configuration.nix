{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  imports = [
    ./hardware-configuration.nix
    ../settings.nix
    ../home-network.nix
    ../gui-core.nix
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
      settings = { General.Enable = "Source,Sink,Media,Socket"; };
    };

    nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
  };

  # SERVICES

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    xrandrHeads = [
      {
        output = "DP-1";
        primary = true;
        monitorConfig = ''
          Option "PreferredMode" "1920x1080"
        '';
      }
    ];

    # Wacom tablet setup
    wacom.enable = true;
  };

  system.stateVersion = "21.11";
}

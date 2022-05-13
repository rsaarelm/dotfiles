{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  imports = [
    ./hardware-configuration.nix
    ../settings.nix
    ../home-network.nix
    ../wayland.nix
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

    # nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
  };


  # NVIDIA

  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
    powerManagement.enable = false;
    modesetting.enable = true;
  };

  environment.sessionVariables = {
    WLR_NO_HARDWARE_CURSORS = "1";
  };


  # MISC

  services.xserver.wacom.enable = true;  # Wacom tablet enabled


  system.stateVersion = "21.11";
}

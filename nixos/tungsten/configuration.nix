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

    # Wacom tablet setup
    wacom.enable = true;

    libinput = {
      enable = true;
    };
  };

  # hiDPI settinps
  hardware.video.hidpi.enable = true;
  services.xserver.dpi = 192;
  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
  };

  system.stateVersion = "21.11";
}

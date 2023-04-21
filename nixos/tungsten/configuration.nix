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

  # PS4 controller support for Steam
  # https://steamcommunity.com/app/221410/discussions/0/1693795812304458372/?l=polish
  # https://discourse.nixos.org/t/steam-steam-run-and-joysticks/1451/2
  services.udev.extraRules = ''
    # This rule is needed for basic functionality of the controller in Steam and keyboard/mouse emulation
    SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", MODE="0666"

    # This rule is necessary for gamepad emulation
    KERNEL=="uinput", MODE="0660", GROUP="rsaarelm", OPTIONS+="static_node=uinput"

    # Valve HID devices over USB hidraw
    KERNEL=="hidraw*", ATTRS{idVendor}=="28de", MODE="0666"

    # Valve HID devices over bluetooth hidraw
    KERNEL=="hidraw*", KERNELS=="*28DE:*", MODE="0666"

    # DualShock 4 over USB hidraw
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="05c4", MODE="0666"

    # DualShock 4 wireless adapter over USB hidraw
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="0ba0", MODE="0666"

    # DualShock 4 Slim over USB hidraw
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="09cc", MODE="0666"

    # DualShock 4 over bluetooth hidraw
    KERNEL=="hidraw*", KERNELS=="*054C:05C4*", MODE="0666"

    # DualShock 4 Slim over bluetooth hidraw
    KERNEL=="hidraw*", KERNELS=="*054C:09CC*", MODE="0666"
  '';

  # hiDPI settinps
  hardware.video.hidpi.enable = true;
  services.xserver.dpi = 192;
  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
  };

  environment.etc = {
    # Logitech Trackman Marble trackball configuration
    #
    # | button       | xorg id | libinput  |
    # |--------------+---------+-----------|
    # | big left     | 1       | BTN_LEFT  |
    # | big right    | 3       | BTN_RIGHT |
    # | little left  | 8       | BTN_SIDE  |
    # | little right | 9       | BTN_EXTRA |
    # Right small button (9) is scroll and middle-click (remapped to 2)

    # FIXME: libinput version's button scrolling stopped working after update to NixOS 20.03
    "X11/xorg.conf.d/50-marblemouse.conf".text = ''
      Section "InputClass"
        Identifier   "Marble Mouse"
        MatchProduct "Logitech USB Trackball"

        #Driver       "libinput"
        #Option       "ScrollMethod"        "button"
        #Option       "ScrollButton"        "9"
        #Option       "MiddleEmulation"     "true"
        #Option       "HorizontalScrolling" "false"
        #Option       "ButtonMapping"       "1 2 3 4 5 6 7 8 2"

        Driver       "evdev"
        Option       "EmulateWheel"       "true"
        Option       "EmulateWheelButton" "9"
        Option       "ButtonMapping"      "1 2 3 4 5 6 7 8 2"
      EndSection
    '';
  };

  system.stateVersion = "21.11";
}

{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common.nix
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
  };

  # SERVICES

  virtualisation.docker.enable = true;

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    # Dual monitor setup
    xrandrHeads = [
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

    # Wacom tablet setup
    wacom.enable = true;

    # Needed for trackball config.
    libinput.enable = true;
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

  system.stateVersion = "18.03";
}

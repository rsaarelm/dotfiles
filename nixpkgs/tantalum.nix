{ pkgs, ... }:

{
  imports = [
    ./common.nix

    ./style/dark-theme.nix
  ];

  programs.autorandr = {
    enable = true;

    hooks.postswitch = {
      notify-i3 = "''${pkgs.i3}/bin/i3-msg restart";
    };

    profiles = {
      default = {
        fingerprint = {
          eDP1 = "00ffffffffffff0006af2d270000000010190104951d117802bc05a2554c9a250e505400000001010101010101010101010101010101143780b87038244010103e0025a510000018000000000000000000000000000000000000000000fe0041554f0a202020202020202020000000fe004231333348414e30322e37200a0082";
        };

        config = {
          HDMI-1.enable = false;
          eDP-1 = {
            enable = true;
            mode = "1920x1080";
            position = "0x0";
            primary = true;
            dpi = 144;
            rate = "60.05";
          };
        };
      };

      tv = {
        fingerprint = {
          HDMI-1 = "00ffffffffffff004dd90085010101010111010380a05a780a0dc9a05747982712484c21080081800101010101010101010101010101023a801871382d40582c450040846300001e011d007251d01e206e28550040846300001e000000fc00534f4e592054562058560a2020000000fd00303e0e460f000a20202020202001bd02032970501f030412130514200716101511020601230907078301000067030c003000b82de3050301023a80d072382d40102c458040846300001e011d00bc52d01e20b828554040846300001e011d8018711c1620582c250040846300009e011d80d0721c1620102c258040846300009e000000000000000000000000000061";
        };

        config = {
          eDP-1.enable = false;
          HDMI-1 = {
            enable = true;
            mode = "1920x1080";
            position = "0x0";
            rate = "60.00";
            dpi = 120;
          };
        };
      };
    };
  };
}

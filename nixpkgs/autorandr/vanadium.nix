{ pkgs, ... }:

{
  programs.autorandr = {
    enable = true;

    hooks.postswitch = {
      notify-i3 = "''${pkgs.i3}/bin/i3-msg restart";
    };

    profiles = {
      default = {
        fingerprint = {
          eDP-1 = "00ffffffffffff000daef21400000000161c0104a51f117802ee95a3544c99260f505400000001010101010101010101010101010101363680a0703820402e1e240035ad10000018000000fe004e3134304843472d4751320a20000000fe00434d4e0a202020202020202020000000fe004e3134304843472d4751320a2000bb";
        };

        config = {
          HDMI-2.enable = false;
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

      docked = {
        fingerprint = {
          HDMI-2 = "00ffffffffffff004c2d700d324e5030221a0103803d23782a5fb1a2574fa2280f5054bfef80714f810081c081809500a9c0b300010108e80030f2705a80b0588a0060592100001e000000fd00184b1e873c000a202020202020000000fc00553238453537300a2020202020000000ff00485450483830303131380a2020012e020334f04d611203130420221f105f605d5e23090707830100006d030c002000803c20106001020367d85dc401788003e30f0104023a801871382d40582c450060592100001e023a80d072382d40102c458060592100001e011d007251d01e206e28550060592100001e565e00a0a0a029503020350060592100001a00000074";
        };

        config = {
          eDP-1.enable = false;
          HDMI-2 = {
            enable = true;
            mode = "1920x1080";
            position = "0x0";
            rate = "23.98";
            dpi = 96;
          };
        };
      };
    };
  };
}

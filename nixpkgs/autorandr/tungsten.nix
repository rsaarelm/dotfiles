{ ... }:

let
  dual-monitors = {
    DP-1 =
      "00ffffffffffff004c2d6f0d324e5030221a0104b53d23783a5fb1a2574fa2280f5054bfef80714f810081c08180a9c0b300950001014dd000a0f0703e80302035005f592100001a000000fd00384b1e873c000a202020202020000000fc00553238453537300a2020202020000000ff00485450483830303131380a2020015c02030ef041102309070783010000023a801871382d40582c45005f592100001e565e00a0a0a02950302035005f592100001a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000bf";
    DVI-I-1 =
      "00ffffffffffff004c2db6023432554809120103803420a02a5ad1a7564b9b24135054bfef80a94081808140714f0101010101010101283c80a070b023403020360006442100001a000000fd00384b1e5111000a202020202020000000fc0053796e634d61737465720a2020000000ff00485331513231363033360a202000c8";
  };
in {
  programs.autorandr = {
    enable = true;

    profiles = {
      hires = {
        fingerprint = dual-monitors;

        config = {
          DVI-I-1.enable = false;
          DP-1 = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "3840x2160";
            rate = "60.00";
            dpi = 192;
          };
        };

        hooks.postswitch = ''
          #!/bin/sh

          #xrandr --dpi 192
          i3-msg restart

          # Mouse speed
          xset mouse 2 2
        '';
      };

      lowres = {
        fingerprint = dual-monitors;

        config = {
          # This is being enabled, but needs to be done in postswitch.
          DVI-I-1.enable = false;
          #DVI-I-1 = {
          #  enable = true;
          #  mode = "1920x1200";
          #  position = "1920x0";
          #  rotate = "left";
          #  rate = "59.95";
          #};
          DP-1 = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "1920x1080";
            rate = "60.00";
            dpi = 96;
          };
        };

        hooks.postswitch = ''
          #!/bin/sh

          # XXX: Do DVI-I-1 juggling here since it won't work right from
          # autorandr.
          xrandr --output DVI-I-1 --off

          #xrandr --dpi 96
          i3-msg restart

          xrandr --output DVI-I-1 --right-of DP-1 --rotate left --auto

          # Mouse speed
          xset mouse 2 2
        '';
      };

      side = {
        fingerprint = dual-monitors;

        config = {
          DVI-I-1 = {
            enable = true;
            mode = "1920x1200";
            position = "0x0";
            rate = "59.95";
            rotate = "left";
            dpi = 96;
          };
          DP-1.enable = false;
        };
      };
    };
  };
}

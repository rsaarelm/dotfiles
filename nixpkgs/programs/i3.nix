{ pkgs, ... }:

{
  xsession = {
    enable = true;

    windowManager.i3 = {
      enable = true;

      config = rec {
        fonts = [
          "DejaVu Sans Mono 10"
        ];

        modifier = "Mod4";

        keybindings = pkgs.lib.mkOptionDefault {
          "${modifier}+s" = "exec rofi -show run";
          "${modifier}+F2" = "exec chromium --incognito";
          "${modifier}+F9" = "exec mpc toggle";
          "${modifier}+F10" = "exec mpc next";

          # Colemak-friendly navigation

          "${modifier}+h" = "focus left";
          "${modifier}+n" = "focus down";
          "${modifier}+e" = "focus up";
          "${modifier}+l" = "focus right";

          "${modifier}+Shift+h" = "move left";
          "${modifier}+Shift+n" = "move down";
          "${modifier}+Shift+e" = "move up";
          "${modifier}+Shift+l" = "move right";

          # Lock / suspend
          "${modifier}+Shift+semicolon" = "exec i3lock -c '#f59ea0'";
          "${modifier}+Shift+slash" =
            "exec \"i3lock -c '#f59ea0' & sleep 2; systemctl suspend\"";

          # Take screenshot
          "${modifier}+Print" =
            "exec \"mkdir -p $HOME/Screenshots; scrot $HOME/Screenshots/`date +%Y%m%dT%H%M%S`.png; xrefresh -solid orange\"";
        };

        floating.criteria = [ { "class" = "Steam"; } { "class" = "magog"; } ];
      };
    };
  };
}

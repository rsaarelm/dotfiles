{ config, pkgs, ... }:

{
  xsession = {
    enable = true;

    windowManager.i3 = {
      enable = true;

      config = rec {
        fonts = {
          names = [ "DejaVu Sans Mono" ];
          size = 10.0;
        };

        modifier = "Mod4";
        workspaceLayout = "tabbed";

        bars = [{
          position = "bottom";
          statusCommand = "i3status-rs config-bottom.toml";
        }];

        keybindings = pkgs.lib.mkOptionDefault {
          "${modifier}+s" = "exec rofi -show run";
          "${modifier}+F2" = "exec chromium-browser --incognito";
          "${modifier}+F9" = "exec mpc toggle";
          "XF86AudioPlay" = "exec mpc toggle";
          "${modifier}+F10" = "exec mpc next";

          "${modifier}+w" = "layout tabbed";
          "${modifier}+f" = "layout toggle split";
          "${modifier}+r" = "layout stacking";
          "${modifier}+p" = ''mode "resize"'';
          "${modifier}+t" = "fullscreen";

          # Colemak-friendly navigation

          "${modifier}+h" = "focus left";
          "${modifier}+n" = "focus down";
          "${modifier}+e" = "focus up";
          "${modifier}+l" = "focus right";

          "${modifier}+Shift+h" = "move left";
          "${modifier}+Shift+n" = "move down";
          "${modifier}+Shift+e" = "move up";
          "${modifier}+Shift+l" = "move right";

          # Scratchpad
          "${modifier}+m" = "scratchpad show";
          "${modifier}+Shift+m" = "move scratchpad";

          # Move workspaces between monitors
          "${modifier}+y" = "move workspace to output left";

          # Lock / suspend
          "${modifier}+Shift+semicolon" = "exec i3lock -c '#5f9ea0'";
          "${modifier}+Shift+slash" =
            ''exec "i3lock -c '#5f9ea0' & sleep 10; systemctl suspend"'';

          # Take screenshot
          "${modifier}+Print" = ''
            exec "mkdir -p $HOME/Screenshots; scrot $HOME/Screenshots/`date +%Y%m%dT%H%M%S`.png; xrefresh -solid orange"'';
        };
      };

      extraConfig = ''
        exec --no-startup-id nitrogen --restore
        exec setxkbmap us -variant colemak
      '';
    };
  };

  programs.i3status-rust = {
    enable = true;

    bars.bottom = {
      icons = "awesome5";
      blocks = [
        { block = "load"; }
        { block = "memory"; }
        { block = "disk_space"; }
        { block = "net"; format = " $icon {$ssid $signal_strength|}"; }
        { block = "battery"; missing_format = ""; }
        { block = "temperature"; format = " $icon $average/$max"; }
        { block = "tea_timer"; increment = 60; done_cmd = "notify-send 'Timer done'"; }
        { block = "sound"; }
        { block = "uptime"; }
        { block = "time"; format = "$icon $timestamp.datetime(f:'%g%V.%u/%m-%d %R') "; interval = 1; }
      ];
    };
  };
}

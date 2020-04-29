{ config, pkgs, ... }:

{
  xsession = {
    enable = true;

    windowManager.i3 = {
      enable = true;

      config = rec {
        fonts = [ "DejaVu Sans Mono 10" ];

        modifier = "Mod4";
        workspaceLayout = "tabbed";

        bars = [{
          position = "bottom";
          statusCommand = "i3status-rs ${config.xdg.configHome}/i3/status.toml";
        }];

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
          "${modifier}+Shift+semicolon" = "exec i3lock -c '#5f9ea0'";
          "${modifier}+Shift+slash" =
            ''exec "i3lock -c '#5f9ea0' & sleep 2; systemctl suspend"'';

          # Take screenshot
          "${modifier}+Print" = ''
            exec "mkdir -p $HOME/Screenshots; scrot $HOME/Screenshots/`date +%Y%m%dT%H%M%S`.png; xrefresh -solid orange"'';
        };
      };
    };
  };

  xdg.configFile."i3/status.toml".text = ''
    [[block]]
    block = "disk_space"
    path = "/"
    alias = "/"
    info_type = "available"
    unit = "GB"
    interval = 20
    warning = 20.0
    alert = 10.0

    [[block]]
    block = "memory"
    display_type = "memory"
    format_mem = "{Mup}%"
    format_swap = "{SUp}%"

    [[block]]
    block = "cpu"
    interval = 1

    [[block]]
    block = "load"
    interval = 1
    format = "{1m}"

    [[block]]
    block = "sound"

    [[block]]
    block = "time"
    interval = 60
    format = "%V.%u/%m-%d %R:%S"
  '';
}

{ config, pkgs, lib, ... }:

{
  wayland.windowManager.sway = {
    enable = true;
    systemdIntegration = true;

    config = {
      fonts = {
        names = [ "DejaVu Sans Mono" ];
        size = 10.0;
      };

      modifier = "Mod4";
      workspaceLayout = "tabbed";
      terminal = "alacritty";
      menu = "rofi";

      # Jump between last two workspaces by tapping current workspace's
      # number.
      workspaceAutoBackAndForth = true;

      input = {
        "type:keyboard" = {
          xkb_layout = "us";
          xkb_variant = "colemak";
        };
      };

      bars = [];

      seat = {
        # Hide mouse cursor after a while.
        "*" = { hide_cursor = "3000"; };
      };

      gaps = {
        smartBorders = "on";
        smartGaps = true;
        inner = 12;
      };

      window.commands = [
        { command = "border pixel 1"; criteria = { class = ".*"; }; }
      ];

      keybindings =
        let mod = config.wayland.windowManager.sway.config.modifier;
        in
        lib.mkOptionDefault {
          "${mod}+F2" = "exec chromium --incognito";

          "${mod}+f" = "layout toggle split";
          "${mod}+d" = "fullscreen toggle";
          "${mod}+s" = "exec rofi -show run";
          "${mod}+p" = "mode resize";

          "${mod}+h" = "focus left";
          "${mod}+l" = "focus right";
          "${mod}+e" = "focus up";
          "${mod}+n" = "focus down";

          "${mod}+Shift+h" = "move left";
          "${mod}+Shift+l" = "move right";
          "${mod}+Shift+e" = "move up";
          "${mod}+Shift+n" = "move down";

          "${mod}+m" = "scratchpad show";
          "${mod}+Shift+m" = "move scratchpad";

          # Lock / suspend
          "${mod}+Shift+semicolon" = "exec swaylock -c '#5f9ea0'";
          "${mod}+Shift+slash" =
            ''exec "swaylock -c '#5f9ea0' & sleep 2; systemctl suspend"'';
      };
    };
  };

  # services.gammastep = {
  #   enable = true;

  #   latitude = 0.0;
  #   longitude = 25.0;

  #   temperature.day = 6500;
  #   temperature.night = 3500;
  # };

  programs.waybar = {
    enable = true;

    settings = [{
      position = "bottom";

      modules-left = [ "sway/workspaces" "sway/mode" "wlr/taskbar" ];
      modules-center = [ "sway/window" ];
      modules-right = [ "cpu" "clock" ];

    }];

    systemd.enable = true;
    systemd.target = "sway-session.target";
  };
}

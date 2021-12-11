{ ... }:

{
  xresources.properties = {
    "*.foreground" = "#222";
    "*.background" = "#999";

    # black
    "*.color0" = "#111";
    "*.color8" = "#444";

    # red
    "*.color1" = "#400";
    "*.color9" = "#a22";

    # green
    "*.color2" = "#140";
    "*.color10" = "#361";

    # yellow
    "*.color3" = "#321";
    "*.color11" = "#652";

    # blue
    "*.color4" = "#128";
    "*.color12" = "#369";

    # magenta
    "*.color5" = "#605";
    "*.color13" = "#827";

    # cyan
    "*.color6" = "#033";
    "*.color14" = "#377";

    # white
    "*.color7" = "#666";
    "*.color15" = "#fff";
  };

  xdg.configFile."nvim/colorscheme.vim".text = ''
    set background=light
  '';

  programs.bat.config.theme = "ansi";

  programs.git.extraConfig = {
    delta = {
      syntax-theme = "GitHub";
      navigate = "true";
    };
  };
}

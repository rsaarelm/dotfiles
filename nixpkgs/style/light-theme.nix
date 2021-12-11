{ ... }:

{
  xresources.properties = {
    "*.foreground" = "#222222";
    "*.background" = "#aaaaaa";

    # black
    "*.color0" = "#111111";
    "*.color8" = "#444444";

    # red
    "*.color1" = "#550000";
    "*.color9" = "#aa2222";

    # green
    "*.color2" = "#225500";
    "*.color10" = "#558822";

    # yellow
    "*.color3" = "#332211";
    "*.color11" = "#887733";

    # blue
    "*.color4" = "#224488";
    "*.color12" = "#3377cc";

    # magenta
    "*.color5" = "#882277";
    "*.color13" = "#cc44bb";

    # cyan
    "*.color6" = "#004444";
    "*.color14" = "#448888";

    # white
    "*.color7" = "#777777";
    "*.color15" = "#ffffff";
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

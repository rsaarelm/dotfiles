{ ... }:

{
  xresources.properties = {
    "*.background" = "#e0e0e0";
    "*.foreground" = "#262626";
    "*.color0" = "#1b1b1b";
    "*.color1" = "#7d1e1e";
    "*.color2" = "#356C00";
    "*.color3" = "#7f702d";
    "*.color4" = "#254d80";
    "*.color5" = "#802576";
    "*.color6" = "#00787a";
    "*.color7" = "#c2c2c2";
    "*.color8" = "#6d6d6d";
    "*.color9" = "#ee3030";
    "*.color10" = "#61be07";
    "*.color11" = "#beb03c";
    "*.color12" = "#3f7ec1";
    "*.color13" = "#c546ba";
    "*.color14" = "#76baba";
    "*.color15" = "#ffffff";
  };

  xdg.configFile."nvim/init.vim".text = ''
    colorscheme gruvbox
    set background=light
  '';

  programs.bat.config.theme = "ansi-light";
}

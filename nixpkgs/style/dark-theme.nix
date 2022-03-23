{ ... }:

{
  xresources.properties = {
    "*background" = "#111111";
    "*foreground" = "#dddddd";
    "*color0" = "#2e3436";
    "*color1" = "#cc0000";
    "*color2" = "#4e9a06";
    "*color3" = "#c4a000";
    "*color4" = "#3465a4";
    "*color5" = "#ff00e4";
    "*color6" = "#00fbff";
    "*color7" = "#d3d7cf";
    "*color8" = "#777780";
    "*color9" = "#ee3030";
    "*color10" = "#8ae234";
    "*color11" = "#fce94f";
    "*color12" = "#729fcf";
    "*color13" = "#b292af";
    "*color14" = "#a2ffff";
    "*color15" = "#eeeeec";
  };

  xdg.configFile."nvim/colorscheme.vim".text = ''
    set background=dark
  '';

  programs.git.extraConfig = {
    delta = {
      plus-style = "syntax #012800";
      minus-style = "syntax #340001";
      syntax-theme = "Monokai Extended";
      navigate = "true";
    };
  };
}

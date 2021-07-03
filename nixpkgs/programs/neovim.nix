{ ... }:

{
  # Manually all my custom cruft that isn't nicely packaged in packages.
  xdg.configFile = {
    "nvim/after/ftplugin/votl.vim".source = ./neovim/after/ftplugin/votl.vim;
    "nvim/after/syntax/votl.vim".source = ./neovim/after/syntax/votl.vim;
    "nvim/colors/gruvbox.vim".source = ./neovim/colors/gruvbox.vim;
    "nvim/colors/jellybeans.vim".source = ./neovim/colors/jellybeans.vim;
    "nvim/ftdetect/jrnl.vim".source = ./neovim/ftdetect/jrnl.vim;
    "nvim/ftdetect/votl.vim".source = ./neovim/ftdetect/votl.vim;
    "nvim/ftplugin/jrnl.vim".source = ./neovim/ftplugin/jrnl.vim;
    "nvim/ginit.vim".source = ./neovim/ginit.vim;
    "nvim/colorscheme.vim".text = "";  # Filled in style nixfile.
    "nvim/core.vim".source = ./neovim/core.vim;
    "nvim/main.vim".source = ./neovim/main.vim;
    "nvim/init.vim".text = ''
      runtime main.vim
    '';
    "nvim/syntax/jrnl.vim".source = ./neovim/syntax/jrnl.vim;
    "nvim/syntax/wikilike.vim".source = ./neovim/syntax/wikilike.vim;
  };

  home.file.".vimoutlinerrc".text = ''
    let g:vo_modules_load = 'checkbox:tags:smart_paste:clock'
  '';
}

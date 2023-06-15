{ ... }:

{
  programs.zathura = {
    enable = true;

    extraConfig = ''
      map <Space> feedkeys "<C-f>"
      map <BackSpace> feedkeys "<C-b>"
      map <S-Space> feedkeys "<C-b>"
      map <Right> feedkeys "<PageDown>"
      map <Left> feedkeys "<PageUp>"
      map c feedkeys "<PageDown>"
      map C feedkeys "<PageUp>"
      map <Button3> navigate next
      set guioptions ""  # Don't show status bar by default
    '';
  };
}

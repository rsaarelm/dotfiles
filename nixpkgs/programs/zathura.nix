{ ... }:

{
  programs.zathura = {
    enable = true;

    extraConfig = ''
      map <Space> feedkeys "<PageDown>"
      map <BackSpace> feedkeys "<PageUp>"
      map <S-Space> feedkeys "<PageUp>"
      map <Right> feedkeys "<C-f>"
      map <Left> feedkeys "<C-b>"
      map c feedkeys "<C-f>"  # Comfy key for left hand
      map C feedkeys "<C-b>"
      map <Button3> navigate next
      set guioptions ""  # Don't show status bar by default
    '';
  };
}

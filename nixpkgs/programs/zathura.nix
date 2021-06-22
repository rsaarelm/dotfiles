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

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = "org.pwmt.zathura.desktop";
      "application/epub+zip" = "org.pwmt.zathura.desktop";
      "application/postscript" = "org.pwmt.zathura.desktop";
      "image/vnd.djvu" = "org.pwmt.zathura.desktop";
      "image/vnd.djvu+multipage" = "org.pwmt.zathura.desktop";

      "image/gif" = "sxiv.desktop";
      "image/png" = "sxiv.desktop";
      "image/jpeg" = "sxiv.desktop";
      "image/bmp" = "sxiv.desktop";
    };
  };
}

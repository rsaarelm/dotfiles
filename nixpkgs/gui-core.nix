# Essential GUI packages
{ pkgs, ... }:

{
  imports = [
    ./cli-core.nix
    ./programs/sway.nix
  ];

  home.packages = with pkgs; [
    keepass
    scrot
    sxiv
  ];

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = "org.pwmt.zathura.desktop";
      "application/postscript" = "org.pwmt.zathura.desktop";
      "image/vnd.djvu" = "org.pwmt.zathura.desktop";
      "image/vnd.djvu+multipage" = "org.pwmt.zathura.desktop";

      "application/epub+zip" = "org.pwmt.zathura.desktop";

      "image/gif" = "sxiv.desktop";
      "image/png" = "sxiv.desktop";
      "image/jpeg" = "sxiv.desktop";
      "image/bmp" = "sxiv.desktop";
    };
  };
}

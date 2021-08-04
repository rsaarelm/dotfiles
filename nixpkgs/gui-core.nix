# Essential GUI packages
{ pkgs, ... }:

{
  imports = [
    ./cli-core.nix
    ./programs/i3.nix
  ];

  home.packages = with pkgs; [
    keepass
    scrot
    sxiv
  ];

  programs.urxvt = {
    enable = true;
    package = pkgs.rxvt_unicode-with-plugins;
    fonts = [
      # Main font
      "xft:opendyslexicmono:size=11"
      # APL font
      "xft:APL385 Unicode"
      # CJK support
      "xft:Noto Sans Mono CJK JP"
      # Icons
      "xft:Symbola"
    ];
    scroll.bar.enable = false;
    scroll.lines = 32000;

    extraConfig = {
      "perl-ext-common" = "default,url-select,resize-font";
      "keysym.M-u" = "perl:url-select:select_next";
      "url-select.launcher" = "chromium-browser --incognito";
      "url-select.underline" = "true";
      "resize-font.step" = "2";
      "keysym.C-equal" = "font-size:increase";
      "keysym.C-minus" = "font-size:decrease";
    };
  };

  services.redshift = {
    enable = true;

    latitude = "60.0";
    longitude = "25.0";

    temperature.day = 6500;
    temperature.night = 3500;

    settings.redshift = {
      brightness-day = "1";
      brightness-night = "0.8";
    };
  };

  services.xcape.enable = true;

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = "org.pwmt.zathura.desktop";
      "application/postscript" = "org.pwmt.zathura.desktop";
      "image/vnd.djvu" = "org.pwmt.zathura.desktop";
      "image/vnd.djvu+multipage" = "org.pwmt.zathura.desktop";

      "application/epub+zip" = "com.github.johnfactotum.Foliate.desktop";

      "image/gif" = "sxiv.desktop";
      "image/png" = "sxiv.desktop";
      "image/jpeg" = "sxiv.desktop";
      "image/bmp" = "sxiv.desktop";
    };
  };
}

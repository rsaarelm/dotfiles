{ config, pkgs, ... }:

{
  imports = [
    ./programs/git.nix
    ./programs/i3.nix
    ./programs/neovim.nix
    ./programs/zsh.nix
  ] ++ (if builtins.pathExists ./local.nix then [ ./local.nix ] else [ ]);
  # Extra settings can be added in non-version-controlled local.nix

  programs.bat.enable = true;

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.ssh = {
    enable = true;

    extraConfig = ''
      AddKeysToAgent yes
    '';
  };

  programs.urxvt = {
    enable = true;
    package = pkgs.rxvt_unicode-with-plugins;
    fonts = [
      # Main font
      "xft:Source Code Pro:size=10"
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

  programs.zathura = {
    enable = true;

    extraConfig = ''
      map <Space> feedkeys "<C-f>"
      map <S-Space> feedkeys "<C-b>"
      map <Left> feedkeys "<PageUp>"
      map <Right> feedkeys "<PageDown>"
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

  services.lorri.enable = true;

  services.mpd = {
    enable = true;
    musicDirectory = "${config.home.homeDirectory}/Music";
    extraConfig = ''
      audio_output {
        type "pulse"
        name "pulse audio"
      }
    '';
  };

  services.redshift = {
    enable = true;

    latitude = "25.0";
    longitude = "60.0";

    temperature.day = 6500;
    temperature.night = 3500;

    brightness.day = "1";
    brightness.night = "0.8";
  };

  services.xcape.enable = true;
}

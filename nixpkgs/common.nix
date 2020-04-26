{ config, pkgs, ... }:

{
  imports = [
    ./packages.nix
    ./programs/git.nix
    ./programs/i3.nix
    ./programs/neovim.nix
    ./programs/zsh.nix
  ];

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
      "xft:Go Mono:size=12"
      # CJK support
      "xft:Noto Sans Mono CJK JP"
      # Emoji support
      "xft:Symbola"
      # Fallback in case the fonts aren't installed
      "9x15"
    ];
    scroll.bar.enable = false;
    scroll.lines = 32000;

    extraConfig = {
      "perl-ext-common" = "default,url-select,resize-font";
      "keysym.M-u" = "perl:url-select:select_next";
      "url-select.launcher" = "chromium --incognito";
      "url-select.underline" = "true";
      "resize-font.step" = "2";
      "keysym.C-equal" = "font-size:increase";
      "keysym.C-minus" = "font-size:decrease";
    };
  };

  programs.zathura = {
    enable = true;

    extraConfig = ''
      map <Space> feedkeys "<PageDown>"
      map <S-Space> feedkeys "<PageUp>"
      map <Left> feedkeys "<S-Space>"
      map <Right> feedkeys "<Space>"
      map <Up> feedkeys "<C-u>"
      map <Down> feedkeys "<C-d>"
      map <S-Up> feedkeys "k"
      map <S-Down> feedkeys "j"
    '';
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

  services.xcape = {
    enable = true;
    mapExpression = {
      Control_L = "Control_L|Escape";
    };
  };
}

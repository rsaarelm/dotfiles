{ pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      # System
      acpi
      acpitool
      at
      bluez
      cachix
      coreutils
      exfat
      inotify-tools
      jmtpfs
      lm_sensors
      openssl
      openvpn
      pciutils
      pkg-config
      psmisc
      usbutils

      # Basic tools
      bat
      btop
      busybox
      caddy
      direnv
      entr
      eza
      fd
      ffmpeg
      file
      fzf
      ghostscript
      git
      git-annex
      glances
      gnumake
      helix
      hledger
      imagemagick
      jq
      jujutsu
      just
      links2
      lynx
      moreutils
      mosh
      ncdu
      neofetch
      nix-index
      nixfmt-rfc-style
      optipng
      p7zip
      pandoc
      pinentry-tty
      pwgen
      (python3.withPackages (
        python-pkgs: with python-pkgs; [
          scipy
          pandas
        ]
      ))
      python3Packages.ipython
      ripgrep
      rumdl # Markdown formatter
      sshfs
      texlive.combined.scheme-small
      tmux
      tokei
      tree-sitter
      unzip
      uv
      wget
      xsel
      xxd
      yewtube
      yt-dlp
      zoxide

      # Programming language stuff
      (agda.withPackages (p: [ p.standard-library ]))
      black # Python formatter
      cargo
      cargo-outdated
      clang-tools
      clippy
      fennel-ls
      fnlfmt # Fennel formatter
      gcc
      lua-language-server
      nodejs
      rust-analyzer
      rust-script
      rustc
      rustfmt
      stack # Haskell thing
      stylua # Lua formatter
    ];

    variables = {
      EDITOR = pkgs.lib.mkOverride 0 "nvim";
      TERMINAL = "alacritty";
    };
  };

  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-tty;
  };
  services.pcscd.enable = true;

  services.displayManager = {
    defaultSession = "none+i3";

    autoLogin.enable = true;
    autoLogin.user = "rsaarelm";
  };

  services.xserver = {
    enable = true;

    displayManager.lightdm.enable = true;
    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        dunst
        i3lock
        i3status-rust
        libnotify
        pavucontrol
        rofi

        alacritty
        blender
        chromium
        gimp
        keepass
        mpv
        scrot
        sxiv
        xorg.xrefresh
        zathura

        godot_4
        gdscript-formatter
      ];
    };

    xkb = {
      layout = "us";
      variant = "colemak";
      options = "ctrl:nocaps";
    };

    desktopManager.xterm.enable = false;

    # Lock screen automatically.
    xautolock = {
      enable = true;
      time = 60; # minutes
      locker = "${pkgs.i3lock}/bin/i3lock --color 002222";
      notify = 20; # seconds
      notifier = "${pkgs.libnotify}/bin/notify-send 'Locking machine in 20 seconds'";
      extraOptions = [ "-detectsleep" ];
    };
  };

  fonts.packages = with pkgs; [
    terminus_font
    intel-one-mono
    nerd-fonts.envy-code-r
    source-code-pro

    # Fonts with extra symbols
    noto-fonts-cjk-sans
  ];

  # Hide immobile mouse cursor.
  services.unclutter-xfixes.enable = true;

  # PipeWire audio
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
}

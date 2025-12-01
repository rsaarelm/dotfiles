{ pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs;
      let neovim = pkgs.neovim.override { vimAlias = true; };
      in [
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
        gcc
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
        neovim
        nix-index
        nixfmt-rfc-style
        nodejs
        optipng
        p7zip
        pandoc
        pinentry-tty
        pwgen
        (python3.withPackages (python-pkgs: with python-pkgs; [
          scipy
          pandas
        ]))
        python3Packages.ipython
        ripgrep
        sshfs
        texlive.combined.scheme-small
        tmux
        tokei
        unzip
        uv
        wget
        xsel
        xxd
        yewtube
        yt-dlp
        zoxide

        # Rust and rust-script stuff
        cargo
        cargo-outdated
        clang
        clippy
        rust-analyzer
        rust-script
        rustc
        rustfmt
      ];

    variables = {
      EDITOR = pkgs.lib.mkOverride 0 "nvim";
      TERMINAL = "st";
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
        (st.overrideAttrs (oldAttrs: rec {
          configFile =
            writeText "config.def.h" (builtins.readFile ./st.config.h);
          postPatch = ''
            ${oldAttrs.postPatch}
             cp ${configFile} config.def.h'';
        }))

        dunst
        i3lock
        i3status-rust
        libnotify
        pavucontrol
        rofi

        alacritty
        blender
        chromium
        godot_4
        gimp
        keepass
        mpv
        neovim-qt
        scrot
        sxiv
        xorg.xrefresh
        zathura
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
      notifier =
        "${pkgs.libnotify}/bin/notify-send 'Locking machine in 20 seconds'";
      extraOptions = [ "-detectsleep" ];
    };
  };

  fonts.packages = with pkgs; [
    terminus_font
    intel-one-mono
    source-code-pro

    # Fonts with extra symbols
    noto-fonts-cjk-sans
    font-awesome
    powerline-fonts
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

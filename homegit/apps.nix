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
        inotifyTools
        jmtpfs
        lm_sensors
        openvpn
        pciutils
        psmisc
        usbutils

        # Basic tools
        bat
        bottom
        direnv
        fd
        ffmpeg
        file
        fzf
        git
        git-annex
        hledger
        imagemagick
        jujutsu
        just
        links2
        mosh
        neofetch
        neovim
        nix-index
        nixfmt-rfc-style
        p7zip
        pandoc
        pwgen
        python3
        python3Packages.ipython
        ripgrep
        sshfs
        tmux
        tokei
        unzip
        wget
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

        chromium
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
    noto-fonts-cjk
    font-awesome
    powerline-fonts
  ];

  # Hide immobile mouse cursor.
  services.unclutter-xfixes.enable = true;

  # Sound on
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;

    # Bluetooth support
    package = pkgs.pulseaudioFull;
  };
}

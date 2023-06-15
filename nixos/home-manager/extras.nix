{ pkgs, ... }:

let
 tt = pkgs.callPackage ./pkgs/tt { };
in {
  imports = [
    ./programs/ranger.nix
  ];

  home.packages = with pkgs; [
    # Utils
    aspell
    aspellDicts.en
    cheat
    delta
    direnv
    du-dust
    duf
    exa
    fd
    fdupes
    ffmpeg
    figlet
    fortune
    gitAndTools.git-annex
    git-annex-utils
    hledger
    htop
    imagemagick
    jmtpfs
    jq
    just
    lsof
    monolith
    mosh
    mpc_cli
    mpd
    ncdu
    neofetch
    optipng
    pandoc
    pdftk
    restic
    ripgrep
    sshfs
    thefuck
    tmux
    tokei
    tree
    unzip
    xxd
    yt-dlp

    # Dev
    clang
    docker
    gnumake
    lldb
    nixfmt
    python3
    python3Packages.ipython

    # X11
    anki
    autorandr
    briss
    foliate
    gimp
    mpv
    neovim-qt
    nitrogen
    notify-osd
    xorg.xrefresh
  ];
}

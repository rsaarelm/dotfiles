{ pkgs, ... }:

let
 tt = pkgs.callPackage ./pkgs/tt { };
in {
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
    figlet
    fortune
    gitAndTools.git-annex
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
    ranger
    restic
    ripgrep
    sshfs
    thefuck
    tmux
    tokei
    tree
    unzip
    xxd
    youtubeDL

    # Dev
    binutils
    clang
    docker
    gnumake
    nixfmt
    python3
    python37Packages.ipython

    # Extra fonts
    apl385

    # X11
    anki
    autorandr
    foliate
    gimp
    mpv
    neovim-qt
    nitrogen
    notify-osd
    xorg.xrefresh
  ];
}

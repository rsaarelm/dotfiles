{ pkgs, ... }:

let
 tt = pkgs.callPackage ./pkgs/tt { };
in {
  home.packages = with pkgs; [
    # Utils
    aspell
    aspellDicts.en
    exa
    fd
    fdupes
    figlet
    fortune
    gitAndTools.git-annex
    hledger
    imagemagick
    jmtpfs
    monolith
    mpc_cli
    mpd
    ncdu
    neofetch
    optipng
    pandoc
    ranger
    restic
    ripgrep
    thefuck
    tokei
    tree
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
    gimp
    keepass
    mpv
    neovim-qt
    nitrogen
    notify-osd
    scrot
    sxiv
    xorg.xrefresh
  ];
}

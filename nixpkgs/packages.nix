{ pkgs, ... }:

let
 tt = pkgs.callPackage ./pkgs/tt { };
in {
  home.packages = with pkgs; [
    # Utils
    aspell
    aspellDicts.en
    direnv
    exa
    fd
    fdupes
    fortune
    gitAndTools.git-annex
    hledger
    htop
    imagemagick
    k2pdfopt
    links
    mosh
    mpc_cli
    mpd
    ncdu
    neofetch
    file
    jmtpfs
    lsof
    optipng
    sshfs
    p7zip
    pandoc
    pwgen
    ranger
    restic
    ripgrep
    tmux
    tokei
    tree
    unzip
    wget
    youtubeDL
    xxd

    tt

    # LaTeX
    (texlive.combine {
      inherit (texlive)
        collection-basic metafont xits collection-bibtexextra
        collection-binextra collection-context collection-formatsextra
        collection-fontutils collection-langenglish collection-latex
        collection-latexextra collection-latexrecommended collection-pictures
        collection-pstricks collection-xetex;
    })

    # Dev
    binutils
    clang
    git
    gnumake
    nixfmt
    python3
    python37Packages.ipython

    # X11
    anki
    autorandr
    gimp
    grafx2
    i3status-rust
    keepass
    mpv
    neovim-qt
    nitrogen
    notify-osd
    rofi
    scrot
    sxiv
    xorg.xrefresh
  ];
}

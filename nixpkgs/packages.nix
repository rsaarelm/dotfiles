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
    figlet
    file
    fortune
    gitAndTools.git-annex
    hledger
    htop
    imagemagick
    jmtpfs
    # FIXME mupdf dependency bitrotted
    # k2pdfopt
    links
    lsof
    monolith
    mosh
    mpc_cli
    mpd
    ncdu
    neofetch
    optipng
    p7zip
    pandoc
    pwgen
    ranger
    restic
    ripgrep
    sshfs
    thefuck
    tmux
    tokei
    tree
    unzip
    wget
    xxd
    youtubeDL

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

    # Extra fonts
    apl385

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

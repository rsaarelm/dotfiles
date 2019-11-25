#!/bin/bash

# Initial setup for apt-based system

sudo apt update

# XXX: Package thefuck is broken on Ubuntu until you install
# python-pkg-resources.
sudo apt install \
    a2ps \
    aspell \
    aspell-en \
    autorandr \
    bluez \
    ccache \
    chromium-browser \
    cmus \
    ctags \
    curl \
    docker \
    fonts-go \
    fonts-mononoki \
    gimp \
    git \
    haskell-stack \
    hledger \
    htop \
    i3 \
    imagemagick \
    ipython3 \
    keepassx \
    links \
    lldb \
    mesa-utils \
    mosh \
    ncdu \
    net-tools \
    neovim \
    neovim-qt \
    pandoc \
    pavucontrol \
    pwgen \
    python3-pip \
    restic \
    rlwrap \
    rofi \
    rxvt-unicode \
    scrot \
    sshfs \
    stow \
    sxiv \
    thefuck \
    tmux \
    unclutter \
    xcape \
    youtube-dl \
    xautolock \
    xfonts-terminus \
    zathura \
    zsh

sudo apt upgrade

# Installs a ghc that can build my tt tool.
if [ ! -f ~/.local/bin/stack ]; then
    stack upgrade
fi

# Tap caps for escape:
#   xcape -e 'Control_L=Escape'

# Reminder to fix caps
if ! grep -q "ctrl:" /etc/default/keyboard; then
    echo "Edit /etc/default/keyboard"
    echo 'add XKBOPTIONS="ctrl:nocaps"'
fi

# Semi-automatically start xcape to treat Control (and Caps after fix) tap as
# Esc.
#
# Xcape has a bug where it occasionally crashes and stops working, so this
# setup makes it restart every time you open a terminal.
if ! grep -q "killall xcape" ~/.zshrc.local; then
    echo "Adding xcape runner to ~/.zshrc.local"
    echo "bash -c \"killall xcape -q; xcape -e 'Control_L=Escape'\"" >> ~/.zshrc.local
fi

if ! update-alternatives --display editor | grep -q "currently.*vim"; then
    echo "Vim isn't set as the default editor, updating..."
    sudo update-alternatives --config editor
fi

if ! update-alternatives --display x-terminal-emulator | grep -q "currently.*urxvt"; then
    echo "Urxvt isn't set as the default terminal, updating..."
    sudo update-alternatives --config x-terminal-emulator
fi

if [ ! -f ~/.ssh/id_rsa ]; then
    echo "SSH key does not exist, generating..."
    ssh-keygen
fi

if ! grep -q "xautolock" ~/.xprofile.local; then
    echo "Activating xautolock"
    echo "xautolock -detectsleep -time 3 -locker 'i3lock' &" >> ~/.xprofile.local
fi

if ! grep -q "unclutter" ~/.xprofile.local; then
    echo "Activating unclutter"
    echo "unclutter &" >> ~/.xprofile.local
fi

# Setup nice terminal colors.
# XXX: Duplicated file from NixOS conf
if [ ! -f ~/.Xdefaults ]; then
    cat <<EOF > ~/.Xdefaults
URxvt.font: xft:mononoki
URxvt.keysym.Mod4-minus: command:\033]50;xft:mononoki\007
URxvt.keysym.Mod4-equal: command:\033]50;xft:mononoki:size=24\007
URxvt.scrollBar: false
URxvt.perl-ext: default,url-select
URxvt.keysym.M-u: perl:url-select:select_next
URxvt.url-select.launcher: chromium --incognito
URxvt.url-select.underline: true
URxvt.saveLines: 32000

*color0: #2e3436
*color1: #cc0000
*color2: #4e9a06
*color3: #c4a000
*color4: #3465a4
*color5: #ff00e4
*color6: #00fbff
*color7: #d3d7cf
*color8: #565654
*color9: #ee3030
*color10: #8ae234
*color11: #fce94f
*color12: #729fcf
*color13: #b292af
*color14: #a2ffff
*color15: #eeeeec

*background: #111111
*foreground: #dddddd
EOF
fi

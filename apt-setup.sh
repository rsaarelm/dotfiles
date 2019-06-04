#!/bin/bash

# Initial setup for apt-based system

sudo apt install \
    autorandr \
    ccache \
    chromium-browser \
    ctags \
    curl \
    docker \
    fonts-hack \
    git \
    haskell-stack \
    hledger \
    i3 \
    imagemagick \
    mesa-utils \
    mosh \
    ncdu \
    neovim \
    neovim-qt \
    rofi \
    rxvt-unicode \
    stow \
    sxiv \
    xcape \
    zathura \
    zsh

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

# Setup nice terminal colors.
# XXX: Duplicated file from NixOS conf
if [ ! -f ~/.Xdefaults ]; then
    cat <<EOF > ~/.Xdefaults
URxvt.font: xft:Hack-10
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

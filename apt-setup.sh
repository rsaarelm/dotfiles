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
    fonts-symbola \
    gimp \
    git \
    haskell-stack \
    hledger \
    htop \
    i3 \
    imagemagick \
    ipython3 \
    k2pdfopt \
    keepassx \
    links \
    lldb \
    mesa-utils \
    mosh \
    ncdu \
    neovim \
    neovim-qt \
    net-tools \
    nitrogen \
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
    tree \
    unclutter \
    xautolock \
    xcape \
    xfonts-terminus \
    youtube-dl \
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
    echo "xautolock -detectsleep -lockaftersleep -time 3 -locker 'i3lock' &" >> ~/.xprofile.local
fi

if ! grep -q "unclutter" ~/.xprofile.local; then
    echo "Activating unclutter"
    echo "unclutter &" >> ~/.xprofile.local
fi

# Setup nice terminal colors.
# XXX: Duplicated file from NixOS conf
if [ ! -f ~/.Xdefaults ]; then
    cat <<EOF > ~/.Xdefaults
URxvt.font:                 xft:mononoki:size=12,xft:Symbola
URxvt.scrollBar:            false
URxvt.saveLines:            32000
URxvt.perl-ext-common:      default,url-select,resize-font
URxvt.keysym.M-u:           perl:url-select:select_next
URxvt.url-select.launcher:  chromium --incognito
URxvt.url-select.underline: true
URxvt.resize-font.step:     2
URxvt.keysym.C-equal:       font-size:increase
URxvt.keysym.C-minus:       font-size:decrease

! special
*.foreground:   #262626
*.background:   #e0e0e0
*.cursorColor:  #262626
! black
*.color0:       #1b1b1b
*.color8:       #6d6d6d
! red
*.color1:       #7d1e1e
*.color9:       #ee3030
! green
*.color2:       #356C00
*.color10:      #61be07
! yellow
*.color3:       #7f702d
*.color11:      #beb03c
! blue
*.color4:       #254d80
*.color12:      #3f7ec1
! magenta
*.color5:       #802576
*.color13:      #c546ba
! cyan
*.color6:       #00787a
*.color14:      #76baba
! white
*.color7:       #c2c2c2
*.color15:      #ffffff
EOF
fi

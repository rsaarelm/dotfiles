#!/bin/sh

# Setup basics for apt-based Linux

# This script should be idempotent, ie. don't do anything that messes things
# up if the same script is run multiple times.

set -e

sudo apt update

sudo apt install -y \
    alacritty \
    cargo \
    chromium-browser \
    fd-find \
    fish \
    fonts-go \
    git \
    git-annex \
    hledger \
    imagemagick \
    ipython3 \
    keepassx \
    links \
    mosh \
    mupdf \
    ncdu \
    neofetch \
    neovim \
    neovim-qt \
    pandoc \
    pavucontrol \
    pwgen \
    restic \
    ripgrep \
    scrot \
    sshfs \
    sxiv \
    tmux \
    yt-dlp \
    zathura \
    zoxide

# X11 stuff
sudo apt install -y \
    i3 \
    rofi \
    unclutter \
    xcape

# Wayland stuff
sudo apt install -y \
    sway

# Reminder to fix caps
if ! grep -q "ctrl:" /etc/default/keyboard; then
    echo "Edit /etc/default/keyboard"
    echo 'add XKBOPTIONS="ctrl:nocaps"'
fi

# XXX: These are repetitive, factor out the patterns?
if ! update-alternatives --display editor | grep -q "currently.*vim"; then
    echo "Vim isn't set as the default editor, updating..."
    sudo update-alternatives --config editor
fi

if ! update-alternatives --display x-window-manager | grep -q "currently.*i3"; then
    echo "I3 isn't set as the default WM, updating..."
    sudo update-alternatives --config x-window-manager
fi

if ! update-alternatives --display x-terminal-emulator | grep -q "currently.*alacritty"; then
    echo "Alacritty isn't set as the default terminal, updating..."
    sudo update-alternatives --config x-terminal-emulator
fi

# XXX: This assumes the shell you're currently running is the default shell,
# might not be the case.
if [ "$SHELL" != "/usr/bin/fish" ]; then
    echo "Changing default shell to fish..."
    chsh -s /usr/bin/fish
fi

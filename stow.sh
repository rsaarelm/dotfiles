#!/bin/sh

# List all subdirectories that should be stowed to $HOME here.

stow core
stow i3

stow mplayer
stow nvim

AUTORANDR=autorandr-`hostname`
if [ -e $AUTORANDR ]; then
    stow $AUTORANDR
fi

stow factorio
stow dungeon-crawl-stone-soup

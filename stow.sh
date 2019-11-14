#!/bin/sh

# List all subdirectories that should be stowed to $HOME here.

stow core
stow config-`hostname`
stow i3

stow nvim

AUTORANDR=autorandr-`hostname`
if [ -e $AUTORANDR ]; then
    stow $AUTORANDR
fi

stow games

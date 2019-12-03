#!/bin/sh

stow -v stow-*
[ -d $(hostname)-stow ] && stow -v $(hostname)-stow

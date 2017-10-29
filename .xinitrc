#!/bin/bash
# Silence bell.
xset b off

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# SSH Keychain
SSH_ASKPASS=/usr/lib/ssh/ssh-askpass keychain ~/.ssh/id_rsa
[ -f ~/.keychain/$HOSTNAME-sh ] && . ~/.keychain/$HOSTNAME-sh 2>/dev/null
[ -f ~/.keychain/$HOSTNAME-sh-gpg ] && . ~/.keychain/$HOSTNAME-sh-gpg 2>/dev/null

redshift -l 60:25 &

dropbox &

# DPI for 4K
#xrandr --dpi 144
# Main 4K monitor on left, side monitor on right
xrandr --auto --output DP-1 --left-of DVI-I-1 --mode 1920x1080
# Pivot
xrandr --output DVI-I-1 --rotate left #--scale 1.5x1.5

# Disable touchpad tapping for 2 seconds when typing
#syndaemon -t -k -i 2 -d

t --daemon &

exec i3

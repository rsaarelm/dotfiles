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

xrandr --dpi 144

# Disable touchpad tapping for 2 seconds when typing
#syndaemon -t -k -i 2 -d

exec i3

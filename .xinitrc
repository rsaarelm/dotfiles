# Silence bell.
xset b off

# Colemak layout
setxkbmap us -variant colemak
# Caps into Ctrl
xmodmap -e 'remove Control = Control_L'
xmodmap -e 'keycode 66 = Control_L Control_L Control_L Control_L'
# Ctrl into backspace
xmodmap -e 'keycode 37 = BackSpace BackSpace BackSpace BackSpace'
xmodmap -e 'add Control = Control_L'

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# SSH Keychain
SSH_ASKPASS=/usr/lib/ssh/ssh-askpass keychain ~/.ssh/id_rsa
[ -f ~/.keychain/$HOSTNAME-sh ] && . ~/.keychain/$HOSTNAME-sh 2>/dev/null
[ -f ~/.keychain/$HOSTNAME-sh-gpg ] && . ~/.keychain/$HOSTNAME-sh-gpg 2>/dev/null

xscreensaver &

redshift -l 60:25 &

exec xmonad

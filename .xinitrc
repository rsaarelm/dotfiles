# Silence bell.
xset b off

# Colemak layout with Caps as Ctrl.
setxkbmap us -variant colemak
xmodmap -e 'keycode 66 = Control_L'
xmodmap -e 'add Control = Control_L'

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# SSH Keychain
SSH_ASKPASS=/usr/lib/ssh/ssh-askpass keychain ~/.ssh/id_rsa
[ -f ~/.keychain/$HOSTNAME-sh ] && . ~/.keychain/$HOSTNAME-sh 2>/dev/null
[ -f ~/.keychain/$HOSTNAME-sh-gpg ] && . ~/.keychain/$HOSTNAME-sh-gpg 2>/dev/null

exec xmonad

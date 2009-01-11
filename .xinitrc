xset b off

xmodmap ~/project/config/custom_keymap.xmodmap
xmodmap ~/project/config/multimedia.xmodmap

xscreensaver &
DISPLAY=:0.1 xrandr -o 1  # Rotate 2nd display

keychain -q --clear ~/.ssh/id_rsa
. ~/.keychain/`uname -n`-sh

# Set to the preferred WM.
ion3
#$HOME/work/core/scripts/status_bar.py | dzen2 -e '' -ta r &
#$HOME/local/bin/xmonad

keychain -k all


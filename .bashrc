alias homegit="GIT_DIR=~/.homegit GIT_WORK_TREE=~ git"
alias ls="ls --color"
alias R="R --no-save"
alias arst="setxkbmap fi"
alias asdf="xset b off;setxkbmap us -variant colemak;xmodmap -e 'remove Control = Control_L' -e 'keycode 66 = Control_L Control_L Control_L Control_L' -e 'keycode 37 = BackSpace BackSpace BackSpace BackSpace' -e 'add Control = Control_L'"
export MAKEFLAGS="-j $(grep -c 'model name' /proc/cpuinfo)"  # Run as many processes as we have cores by default.

export PATH="$HOME/bin:$HOME/local/bin:$PATH:$HOME/work/config/scripts"
export EDITOR="vim"
export VISUAL="vim"
export HISTCONTROL=ignorespace

export PS1="[\$(date +%H:%M) \u@\h \W]$ "

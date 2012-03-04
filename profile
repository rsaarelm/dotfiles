alias ls="ls --color"
alias ocaml="rlwrap ocaml"
alias sml="rlwrap sml"
alias R="R --no-save"
alias arst="setxkbmap fi"
alias asdf="setxkbmap us -variant colemak;xmodmap -e 'remove Control = Control_L' -e 'keycode 66 = Control_L Control_L Control_L Control_L' -e 'keycode 37 = BackSpace BackSpace BackSpace BackSpace' -e 'add Control = Control_L'"

export PATH="$HOME/bin:$HOME/local/bin:$PATH:$HOME/work/config/scripts"
export EDITOR="vim"
export VISUAL="vim"
# export LD_LIBRARY_PATH="/usr/local/lib:$HOME/local/lib:."

export PS1="[$(date +%H:%M) \u@\h \W]$ "

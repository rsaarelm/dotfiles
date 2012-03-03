alias ls="ls --color"
alias ocaml="rlwrap ocaml"
alias sml="rlwrap sml"
alias R="R --no-save"
alias arst="setxkbmap fi"
alias asdf="setxkbmap us variant colemak"

export PATH="$HOME/bin:$HOME/local/bin:$PATH:$HOME/work/config/scripts"
export EDITOR="vim"
export VISUAL="vim"
# export LD_LIBRARY_PATH="/usr/local/lib:$HOME/local/lib:."

export PS1="\[\033[1;32m\]\$(date +%H:%M)\[\033[0m\] \u@\h \W$ "

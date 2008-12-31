alias ls="ls --color"
alias ocaml="rlwrap ocaml"
alias sml="rlwrap sml"
alias R="R --no-save"
alias ls="ls --color"
alias tstamp="date +%Y-%m-%d"
alias shuffle="python -c \"import sys; import random; args = sys.argv[1:]; random.shuffle(args); print ' '.join(args)\""

export PATH="./:$HOME/bin:$HOME/local/bin:$PATH"
export EDITOR="vim"
export VISUAL="vim"
export LD_LIBRARY_PATH="/usr/local/lib:$HOME/local/lib:."

# Grab all jar files in ~/local/lib into CLASSPATH
for x in $HOME/local/lib/*.jar
do
  export CLASSPATH="$CLASSPATH:$x"
done

# Fix the zsh home/end behavior
case $SHELL in (*zsh)
    bindkey "\eOH" beginning-of-line
    bindkey "\eOF" end-of-line
    bindkey "\e[1~" beginning-of-line
    bindkey "\e[4~" end-of-line
    export PROMPT="%n@%m $(print '%{\e[1;32m%}%w %T%{\e[0m%}') %# " ;;
esac

# Colored prompt for Bash (currently a later iteration from the Zsh prompt
# above)

case $SHELL in (*bash)
    if [ $TERM = "rxvt-unicode" ]; then
      # rxvt seems to have a bug at startup when using escape sequences.
      # So we won't use the pretty colors then.
      export PS1="\$(date +%H:%M) \u@\h \W$ "
    else
      export PS1="\[\033[1;32m\]\$(date +%H:%M)\[\033[0m\] \u@\h \W$ "
    fi
esac

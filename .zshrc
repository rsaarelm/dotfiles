[[ -s $HOME/.zshrc.local ]] && source "$HOME/.zshrc.local"

alias homegit="GIT_DIR=~/.homegit GIT_WORK_TREE=~ git"
alias ls="ls --color=auto"
alias R="R --no-save" # Used as quick calculator, make it easy to exit.
alias arst="setxkbmap us -variant intl"
alias asdf="xset b off;setxkbmap us -variant colemak;xmodmap -e 'remove Control = Control_L' -e 'keycode 66 = Control_L Control_L Control_L Control_L' -e 'keycode 37 = BackSpace BackSpace BackSpace BackSpace' -e 'add Control = Control_L'"

# Log out from the shell when starting X
alias startx="startx&disown;exit"

export PATH="$PATH:$HOME/bin:$HOME/.cabal/bin"
export EDITOR=vim
export VISUAL=vim

setopt hist_ignore_space

autoload -U colors && colors # Enable colors in prompt
setopt prompt_subst

git_prompt() {
  local git_where="$(timeout 0.1 git name-rev --name-only --no-undefined --always HEAD 2> /dev/null)"
  if [[ ! -n $git_where ]]; then return; fi
  local col="%{$reset_color%}"
  local gits="$(timeout 0.3 git status -unormal 2>&1)"
  if [[ -z $gits ]]; then; col="%{$fg_bold[black]%}" # Timed out
  elif [[ $gits =~ "Untracked files" ]]; then
    if [[ $gits =~ "Changes not staged" ]]; then; col="%{$fg[magenta]%}"
    elif [[ $gits =~ "Changes to be committed" ]]; then; col="%{$fg[cyan]%}"
    else; col="%{$fg[blue]%}"
    fi
  else
    if [[ $gits =~ "Changes not staged" ]]; then; col="%{$fg[yellow]%}"
    elif [[ $gits =~ "Changes to be committed" ]]; then; col="%{$fg[green]%}"
    fi
  fi
  local prompt=
  echo $col"±$git_where%{$reset_color%} "
}

user_color() { if [[ "$EUID" = 0 ]]; then echo "red"; else; echo "green"; fi }

PS1='%m %{$fg[$(user_color)]%}%~ %#%{$reset_color%} '
RPS1='$(git_prompt)%{$fg[$(user_color)]%}%n%{$reset_color%} %{$fg[cyan]%}%D{%H:%M:%S}%{$reset_color%}'

# Simpler prompt when using Midnight Commander
if ps $PPID | grep mc; then
    PS1="%~ %# "
    RPS1=
fi

# Haven't learned to comfortably switch modes in the ZLE, making it use the
# Emacs mode instead of the Vim one.
bindkey -e

autoload -U zmv

is_cygwin() {
    [[ `uname -o` == 'Cygwin' ]]
}

if is_cygwin; then
    # Revert Cygwin's path mangling for P4.
    alias p4='PWD=$(cygpath --windows --absolute .) p4'
fi

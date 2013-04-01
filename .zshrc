alias homegit="GIT_DIR=~/work/homegit/.git GIT_WORK_TREE=~ git"
alias ls="ls --color=auto"
alias arst="setxkbmap us -variant intl"
alias asdf="xset b off;setxkbmap us -variant colemak;xmodmap -e 'remove Control = Control_L' -e 'keycode 66 = Control_L Control_L Control_L Control_L' -e 'keycode 37 = BackSpace BackSpace BackSpace BackSpace' -e 'add Control = Control_L'"

export PATH="$PATH:$HOME/bin"
export EDITOR=vim
export VISUAL=vim

setopt hist_ignore_space

autoload -U colors && colors # Enable colors in prompt
setopt prompt_subst

git_prompt() {
  local git_where="$(git name-rev --name-only --no-undefined --always HEAD 2> /dev/null)"
  if [[ ! -n $git_where ]]; then return; fi
  local col="%{$reset_color%}"
  local gits="$(git status -unormal 2>&1)"
  if [[ $gits =~ "Untracked files" ]]; then
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

user_color() { if [[ "$EUID" = 0 ]]; then echo "red"; else; echo "blue"; fi }

PS1='%~ %# '
RPS1='$(git_prompt)%{$fg[$(user_color)]%}%n@%m%{$reset_color%} %{$fg[cyan]%}%T%{$reset_color%}'

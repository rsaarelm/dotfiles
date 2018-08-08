# Keyboard togglers
alias arst="setxkbmap us -variant intl"
alias asdf="setxkbmap us -variant colemak"

export RUST_SRC_PATH=/home/rsaarelm/rustc-nightly/src

setopt hist_ignore_space

autoload -U colors && colors # Enable colors in prompt
setopt prompt_subst

git_prompt() {
  local git_where="$(timeout 0.1 git name-rev --name-only --no-undefined --always HEAD 2> /dev/null)"
  if [[ ! -n $git_where ]]; then return; fi
  local col="%{$reset_color%}"
  local gits="$(timeout 0.3 git status -unormal 2>&1)"
  if [[ -z $gits ]]; then; col="%{$fg_bold[black]%}" # Timed out
  else
    # Untracked files are messy, but don't really matter that much. Notify
    # about them with a dimmer status color.
    if [[ $gits =~ "Untracked files" ]]; then local u=1; fi
    if [[ $gits =~ "Changes not staged" ]]; then local ns=1; fi
    if [[ $gits =~ "Changes to be committed" ]]; then local c=1; fi
    if [[ $gits =~ "Unmerged paths" ]]; then local m=1; fi

    # Unmerged stuff, go fix.
    if [[ $u && $m ]]; then col="%{$fg[red]%}";
    elif [[ $m ]]; then col="%{$fg_bold[red]%}";
    # Changes partially committed, better get that figured out.
    elif [[ $u && $ns && $c ]]; then col="%{$fg[magenta]%}";
    elif [[ $ns && $c ]]; then col="%{$fg_bold[magenta]%}";
    # Things are changed, but not ready to commit yet.
    elif [[ $u && $ns ]]; then col="%{$fg[yellow]%}";
    elif [[ $ns ]]; then col="%{$fg_bold[yellow]%}";
    # Commit is good to go.
    elif [[ $u && $c ]]; then col="%{$fg[green]%}";
    elif [[ $c ]]; then col="%{$fg_bold[green]%}";
    # Just some untracked files around.
    elif [[ $u ]]; then col="%{$fg[blue]%}";
    fi
  fi
  local prompt=
  echo $col"±$git_where%{$reset_color%} "
}

clock_prompt() {
  echo "%{$fg[cyan]%}%D{%H:%M:%S}%{$reset_color%}"
}

ret_prompt() {
  echo "%(?..%{$fg[yellow]%}%?%{$reset_color%} )"
}

user_color() { if [[ "$EUID" = 0 ]]; then echo "red"; else; echo "green"; fi }

PS1='%m $(ret_prompt)%{$fg[$(user_color)]%}%~ %#%{$reset_color%} '
RPS1='$(git_prompt)%{$fg[$(user_color)]%}%n%{$reset_color%} $(clock_prompt)'

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

if command -v thefuck > /dev/null; then
    eval $(thefuck --alias)
fi

[[ -s $HOME/.zshrc.local ]] && source "$HOME/.zshrc.local"

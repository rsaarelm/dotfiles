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
    eval $(PYTHONPATH='' thefuck --alias)
fi

# Clean up old files from tmp dir. Move them to trash/ instead of deleting
# outright in case things go wrong.
# XXX: This should be a daily cronjob instead.
mkdir -p $HOME/trash
mkdir -p $HOME/tmp
find $HOME/tmp/ -mindepth 1 -maxdepth 1 -mtime +30 -exec echo "Scheduled cleanup: Moving {} to ~/trash/" \; -exec mv {} $HOME/trash \;

# Work stuff, symlink ~/dayjob to work folder with its own todo.txt
alias wtt="tt --prefix ~/dayjob"
alias workhours="wtt timeclock | hledger -f - balance -p 'daily this week'"

# 48 min + 12 min work cycle. Put this in a loop for repeated work.
function pomodoro() {
    # Optionally specify duration and break using arguments, eg.
    #   pomodoro 25 5
    # for the classic half-hour cycle
    if [ "$1" != "" ]; then DURATION="$1" else DURATION=48 fi
    if [ "$2" != "" ]; then BREAK="$2" else BREAK=12 fi

    # Start cmus if needed
    if ! pgrep -x cmus > /dev/null; then
        urxvt -name cmus -e cmus&
        sleep 0.5
    fi
    # Catch Ctrl-C and turn off the music
    trap "{ cmus-remote -U; return; }" SIGINT
    echo "$DURATION minutes of work starting `date +%H:%M`"
    cmus-remote -p
    sleep "$DURATION"m
    echo "$BREAK minutes of break starting `date +%H:%M`"
    cmus-remote -U
    sleep "$BREAK"m
}

# Do repeating by-the-clock pomodoro cycle from https://guzey.com/productivity/
#
# Using 48/12 minutes instead of 25/5, breaks aren't specified.
function pomodoros() {
    # Start cmus if needed
    if ! pgrep -x cmus > /dev/null; then
        urxvt -name cmus -e cmus&
        sleep 0.5
    fi
    # Catch Ctrl-C and turn off the music
    trap "{ cmus-remote -U; return; }" SIGINT

    while [ 1 ]
    do
        # Seconds from midnight
        (( seconds = `date -d "1970-01-01 UTC $(date +%T)" +%s` ))
        (( seconds_in_pomodoro = $seconds % 3600 ))
        if (( $seconds_in_pomodoro < 720 ))
        then
            (( break_time = 720 - $seconds_in_pomodoro ))
            echo "Break for $(date -d@$break_time -u +%M:%S)"
            (( break_time = $break_time + 1 )) # Go past threshold time
            cmus-remote -U
            sleep $break_time
        else
            (( work_time = 3600 - $seconds_in_pomodoro ))
            echo "Work for $(date -d@$work_time -u +%M:%S)"
            (( work_time = $work_time + 1 )) # Go past threshold time
            cmus-remote -p
            sleep $work_time
        fi
    done
}

# Sleep until exact time
function sleepuntil () {
    SECONDS=$(expr `date -d "$1" +%s` - `date -d "now" +%s`)
    echo "Sleeping for $SECONDS s"
    sleep $SECONDS
}

# Download video into ogg file
alias audio-dl='youtube-dl -x --audio-format vorbis'

# Personal wiki speed dial
alias wiki="pushd -q ~/notes/wiki; vim FrontPage.otl; popd -q"

# Extra environment variables can be listed in ~/.env-settings in
# VAR=value
# lines
if [[ -s $HOME/.env-settings ]]; then
    set -o allexport
    source $HOME/.env-settings
    set +o allexport
fi

# Optional file for local configuration
[[ -s $HOME/.zshrc.local ]] && source "$HOME/.zshrc.local"

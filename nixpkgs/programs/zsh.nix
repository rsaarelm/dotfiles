{ ... }:

# FIXME: use `history.ignoreSpace = true;` instead of setopt in initExtra
{
  programs.zsh = {
    enable = true;

    autocd = true;
    defaultKeymap = "emacs";

    shellAliases = {
      arst = "setxkbmap us -variant intl";
      asdf = "setxkbmap us -variant colemak";

      # tt aliases
      wtt = "tt --prefix ~/dayjob";
      weekhours = "wtt timeclock | hledger -f - balance -p 'daily this week'";
      lastweekhours =
        "wtt timeclock | hledger -f - balance -p 'daily last week'";

      # Stochastic time tracking
      t = "tt log-ping 45";
      tp = "tt missed-pings 45";

      # Download video into ogg file
      audio-dl = "youtube-dl -x --audio-format vorbis";

      # Misc
      wet = "curl wttr.in";
      d = "notify-send done";

      # Enable nix stuff on non-NixOS machine
      local-nix = ". $HOME/.nix-profile/etc/profile.d/nix.sh";

      burner-chromium = "chromium-browser --user-data-dir=`mktemp -d`";
    };
    initExtra = ''
      # Convenience viewer function, does not lock shell
      function v () { xdg-open $* 2> /dev/null &! }

      # Nice simple prompt

      #       [- path    -][- error code -]
      PROMPT='%F{green}%~%f%(?.. %F{red}%?%f) %F{green}%%%f '

      #        [- username   -] [- clock            -]
      RPROMPT='%F{green}%n@%m%f %F{cyan}%D{%H:%M:%S}%f'

      eval $(thefuck --alias)

      setopt histignorespace

      # Shortcut for running a nix-shell installed program
      function n() {
        nix-shell -p $1 --run "$*"
      }

      # Do repeating by-the-clock pomodoro cycle from https://guzey.com/productivity/
      #
      # Toggles play state for both work and break, manually set play state to
      # choose between music-when-working and music-on-break.
      function pomodoros() {
          # Optionally specify duration and break using arguments, eg.
          #   pomodoros 25 5
          # for a half-hour cycle
          if [ "$1" != "" ]; then (( DURATION=$1*60 )) else (( DURATION=25*60 )) fi
          if [ "$2" != "" ]; then (( BREAK=$2*60 )) else (( BREAK=5*60 )) fi

          (( POMODORO_LENGTH = $DURATION + $BREAK ))

          # Catch Ctrl-C and turn off the music
          # trap "{ mpc pause; return; }" SIGINT

          while [ 1 ]
          do
              # Seconds from midnight
              (( seconds = `date -d "1970-01-01 UTC $(date +%T)" +%s` ))
              (( seconds_in_pomodoro = $seconds % $POMODORO_LENGTH ))
              if (( $seconds_in_pomodoro < $BREAK ))
              then
                  notify-send break
                  (( break_time = $BREAK - $seconds_in_pomodoro ))
                  echo "Break for $(date -d@$break_time -u +%M:%S)"
                  (( break_time = $break_time + 0.1 )) # Go past threshold time
                  # mpc toggle
                  mpv ~/Music/noise/whistle.wav &
                  sleep $break_time
              else
                  notify-send work
                  (( work_time = $POMODORO_LENGTH - $seconds_in_pomodoro ))
                  echo "Work for $(date -d@$work_time -u +%M:%S)"
                  (( work_time = $work_time + 0.1 )) # Go past threshold time
                  # mpc toggle
                  mpv ~/Music/noise/bell.wav &
                  sleep $work_time
              fi
          done
      }
    '';
  };
}

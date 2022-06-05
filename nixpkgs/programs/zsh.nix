{ ... }:

# FIXME: use `history.ignoreSpace = true;` instead of setopt in initExtra
{
  programs.zsh = {
    enable = true;

    autocd = true;
    defaultKeymap = "emacs";

    shellAliases = {
      # tt aliases
      wtt = "tt --prefix ~/dayjob";
      weekhours = "wtt timeclock | hledger -f - balance -p 'daily this week'";
      lastweekhours =
        "wtt timeclock | hledger -f - balance -p 'daily last week'";

      # Stochastic time tracking
      t = "tt log-ping 45";
      tp = "tt missed-pings 45";

      # Download video into ogg file
      audio-dl = "yt-dlp -x --audio-format vorbis";

      # Misc
      wet = "curl wttr.in";
      d = "notify-send done";
      exan = "exa -snew";

      # Enable nix stuff on non-NixOS machine
      local-nix = ". $HOME/.nix-profile/etc/profile.d/nix.sh";

      burner-chromium = "chromium-browser --user-data-dir=`mktemp -d`";
      music-chromium = "chromium-browser --user-data-dir=$HOME/music-chromium";
    };

    envExtra = ''
      if [[ -f ~/.zshenv.local ]]; then
        source ~/.zshenv.local
      fi
    '';

    initExtra = ''
      # Convenience viewer function, does not lock shell
      function v () {
        if [ -f $1 ]; then
          FILEPATH="$(realpath $1)"
          FILENAME="$(basename $1)"
          LINKPATH="$(realpath ~/recently-read/$FILENAME)"

          xdg-open $FILEPATH 2> /dev/null &!

          if [ $FILEPATH != $LINKPATH ]; then
            # Keep track of files opened last to get a "recently read" queue
            mkdir -p ~/recently-read
            rm -f $LINKPATH
            ln -s $FILEPATH $LINKPATH
          else
            echo "Physical file already in ~/recently-read/, not clobbering."
            touch $FILEPATH
          fi

          # Also log them so we retain earlier read times when a document is
          # re-read.
          echo "$FILENAME\t`date -Imin`\t`sha1sum $1 | cut -d ' ' -f 1`" >> ~/recently-read/log.txt

          echo "Tagged $FILENAME as recently read"
        fi
      }

      eval $(thefuck --alias)

      setopt histignorespace

      # Shortcut for running a nix-shell installed program
      function n() {
        case $1 in
          # Handle packages where the binary name differs from package name.
          # (Just manually add things I find myself using here)
          "Discord")          nix-shell -p discord --run "$*" ;;
          "FBReader")         nix-shell -p fbreader --run "$*" ;;
          "btm")              nix-shell -p bottom --run "$*" ;;
          "cataclysm")        nix-shell -p 'cataclysm-dda.override { tiles = false; }' --run "$*" ;;
          "cataclysm-tiles")  nix-shell -p cataclysm-dda --run "$*" ;;
          "getgbook")         nix-shell -p getxbook --run "$*" ;;
          "glxgears")         nix-shell -p glxinfo --run "$*" ;;
          "perf")             nix-shell -p linuxPackages.perf --run "$*" ;;
          "wtfutil")          nix-shell -p wtf --run "$*" ;;
          "x64")              nix-shell -p vice --run "$*" ;;
          "xev")              nix-shell -p xorg.xev --run "$*" ;;

          # Default case.
          *) nix-shell -p $1 --run "$*" ;;
        esac
      }
    '';
  };
}

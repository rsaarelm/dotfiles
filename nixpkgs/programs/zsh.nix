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
      audio-dl = "youtube-dl -x --audio-format vorbis";

      # Misc
      wet = "curl wttr.in";
      d = "notify-send done";
      exan = "exa -snew";

      # Enable nix stuff on non-NixOS machine
      local-nix = ". $HOME/.nix-profile/etc/profile.d/nix.sh";

      burner-chromium = "chromium-browser --user-data-dir=`mktemp -d`";
    };
    initExtra = ''
      # Convenience viewer function, does not lock shell
      function v () {
        if [ -f $1 ]; then
          xdg-open "`realpath $1`" 2> /dev/null &!

          # Keep track of files opened last to get a "recently read" queue
          mkdir -p ~/recently-read
          FILE="`realpath $1`"
          rm -f ~/recently-read/"`basename $1`"
          ln -s $FILE ~/recently-read/"`basename $1`"

          # Also log them so we retain earlier read times when a document is
          # re-read.
          echo "`basename $1`\t`date -Imin`\t`sha1sum $1 | cut -d ' ' -f 1`" >> ~/recently-read/log.txt

          echo "Tagged `basename $1` as recently read"
        fi
      }

      eval $(thefuck --alias)

      setopt histignorespace

      # Shortcut for running a nix-shell installed program
      function n() {
        case $1 in
          # Handle packages where the binary name differs from package name.
          # (Just manually add things I find myself using here)
          "Discord") nix-shell -p discord --run "$*" ;;
          "btm") nix-shell -p bottom --run "$*" ;;
          "getgbook") nix-shell -p getxbook --run "$*" ;;
          "glxgears") nix-shell -p glxinfo --run "$*" ;;
          "wtfutil") nix-shell -p wtf --run "$*" ;;
          "x64") nix-shell -p vice --run "$*" ;;
          "xev") nix-shell -p xorg.xev --run "$*" ;;

          # Default case.
          *) nix-shell -p $1 --run "$*" ;;
        esac
      }
    '';
  };
}

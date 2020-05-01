{ ... }:

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

      # Training for new commands
      ls = "exa";
      find = "echo try fd";
      grep = "echo try rg";
      cat = "echo try bat";
      wc = "echo try tokei";
    };
    initExtra = ''
      # Convenience viewer function, does not lock shell
      function v () { zathura $* 2> /dev/null &! }

      # Nice simple prompt

      #       [- path    -][- error code -]
      PROMPT='%F{green}%~%f%(?.. %F{red}%?%f) > '

      #        [- username   -] [- clock            -]
      RPROMPT='%F{green}%n@%m%f %F{cyan}%D{%H:%M:%S}%f'

      eval $(thefuck --alias)
    '';
  };
}

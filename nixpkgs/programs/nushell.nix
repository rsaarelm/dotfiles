{ ... }:

{
  programs.nushell = {
    enable = true;

    envFile.text = ''
      def create_right_prompt [] {
        let time_segment = ([
            (date now | date format '%Y-%m-%d %H:%M:%S')
        ] | str join)

        $time_segment
      }
      let-env PROMPT_COMMAND_RIGHT = { create_right_prompt }
    '';

    # TODO 2023-02-24 Use home-manager's enableNushellIntegration instead of the pre_prompt hook when it's available in nixpkgs version
    configFile.text = ''
      let-env config = {
        show_banner: false

        hooks: {
          # Direnv integration
          pre_prompt: [{
            code: "
              let direnv = (direnv export json | from json)
              let direnv = if ($direnv | length) == 1 { $direnv } else { {} }
              $direnv | load-env
            "
          }]
        }
      }

      # Oneshot runner for uninstalled NixOS programs.
      def nx [
        ...args: string  # Command line invocation of the program
      ] {
        # Replace package names that are different from the executable name
        let package_name = ({
          'Discord': 'discord'
          'FBReader': 'fbreader'
          'btm': 'bottom'
          'cataclysm-tiles': 'cataclysm-dda'
          'keeper': 'keeperrl'
          'getgbook': 'getxbook'
          'glxgears': 'glxinfo'
          'perf': 'linuxPackages.perf'
          'wtfutil': 'wtf'
          'x64': 'vice'
          'xev': 'xorg.xev'
        } | get -i $args.0 | default $args.0)
        let cmd = ($args | str join ' ')
        ^nix-shell -p $package_name --run $"($cmd)"
      }

      alias burner-chromium = chromium $"--user-data-dir=(mktemp -d)"
      alias music-chromium = chromium $"--user-data-dir=($env.HOME)/music-chromium"
    '';
  };
}

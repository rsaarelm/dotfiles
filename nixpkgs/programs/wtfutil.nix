{ pkgs, ... }:

{
  home.packages = with pkgs; [
    wtf
  ];

  xdg.configFile."wtf/config.yml".text = ''
    wtf:
      grid:
        columns: [40, 40, 40, 40]
        rows: [10, 10, 10, 10, 4]
      refreshInterval: 1
      mods:
        syslog:
          title: "systemd"
          cmd: "journalctl"
          args: ["-o", "cat", "--no-full", "-r", "-n100"]
          enabled: true
          position:
            top: 0
            left: 0
            height: 4
            width: 1
          refreshInterval: 10
          type: cmdrunner

        clocks:
          title: "Time"
          type: clocks
          colors:
            rows:
              even: "lightblue"
              odd: "white"
          enabled: true
          locations:
            GMT: "Etc/GMT"
            Helsinki: "Europe/Helsinki"
            New_York: "America/New_York"
            San_Francisco: "America/San_Francisco"
          position:
            top: 0
            left: 1
            height: 1
            width: 1
          refreshInterval: 15
        weather:
          title: "weather"
          cmd: "sh"
          args: ["-c", "curl -s wttr.in | head -n 7"]
          enabled: true
          type: "cmdrunner"
          position:
            top: 1
            left: 1
            height: 1
            width: 1
          refreshInterval: 300
        ipinfo:
          colors:
            name: "lightblue"
            value: "white"
          enabled: true
          position:
            top: 2
            left: 1
            height: 1
            width: 1
          refreshInterval: 150
        security:
          enabled: true
          position:
            top: 3
            left: 1
            height: 1
            width: 1
          refreshInterval: 500

        resourceusage:
          cpuCombined: false
          enabled: true
          position:
            top: 0
            left: 2
            height: 1
            width: 1
          refreshInterval: 1
          showCPU: true
          showMem: true
          showSwp: true

        uptime:
          cmd: "uptime"
          args: [""]
          enabled: true
          position:
            top: 4
            left: 0
            height: 1
            width: 2
          refreshInterval: 30
          type: cmdrunner

        feedreader:
          enabled: true
          feeds:
          - https://lobste.rs/rss
          - https://news.ycombinator.com/rss
          - https://boards.4channel.org/g/index.rss
          - https://feeds.yle.fi/uutiset/v1/recent.rss?publisherIds=YLE_UUTISET
          feedLimit: 8
          position:
            top: 0
            left: 3
            width: 1
            height: 4
  '';
}

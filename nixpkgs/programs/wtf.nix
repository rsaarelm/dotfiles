{ pkgs, ... }:

let
  # FIXME: This is a bugfix, remove it when upstrean wtf gets updated.
  wtf = pkgs.wtf.overrideAttrs(attr: {
    patches = [./wtf.patch];
  });
in
{
  home.packages = [
    wtf
  ];

  # Don't specify a grid, it'll adapt to terminal dimensions
  xdg.configFile."wtf/config.yml".text = ''
    wtf:
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
            height: 8
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
            "New York": "America/New_York"
            "Los Angeles": "America/Los_Angeles"
          position:
            top: 0
            left: 1
            height: 2
            width: 1
          refreshInterval: 15
        weather:
          title: "weather"
          cmd: "sh"
          args: ["-c", "curl -s wttr.in | head -n 7"]
          enabled: true
          type: "cmdrunner"
          position:
            top: 2
            left: 1
            height: 2
            width: 1
          refreshInterval: 300
          focusable: false
        ipinfo:
          colors:
            name: "lightblue"
            value: "white"
          enabled: true
          position:
            top: 4
            left: 1
            height: 2
            width: 1
          refreshInterval: 150
        security:
          enabled: true
          position:
            top: 6
            left: 1
            height: 2
            width: 1
          refreshInterval: 500

        resourceusage:
          cpuCombined: false
          enabled: true
          position:
            top: 0
            left: 2
            height: 2
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
            top: 8
            left: 0
            height: 1
            width: 2
          refreshInterval: 30
          type: cmdrunner
          focusable: false

        feedreader:
          enabled: true
          feeds:
          - https://lobste.rs/rss
          - https://news.ycombinator.com/rss
          - https://feeds.yle.fi/uutiset/v1/recent.rss?publisherIds=YLE_UUTISET
          position:
            top: 0
            left: 3
            width: 1
            height: 8
  '';
}

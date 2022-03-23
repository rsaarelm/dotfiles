{ ... }:

{
  programs.chromium = {
    enable = true;
    extensions = [
      # uBlock Origin
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; }
      # Reddit Enhancement Suite
      { id = "kbmfpngjjgdllneeigpgjifpgocmfgmb"; }
      # Old Reddit Redirect
      { id = "dneaehbmnbhcippjikoajpoabadpodje"; }
      # Nitter redirect
      { id = "mohaicophfnifehkkkdbcejkflmgfkof"; }
      # (Was updated into uselessness)
      # # Wayback Machine
      # { id = "fpnmgdkabkmnadcjpehmlllkndpkmiak"; }
    ];
  };
}

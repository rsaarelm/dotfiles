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
      # Wayback Machine
      { id = "fpnmgdkabkmnadcjpehmlllkndpkmiak"; }
    ];
  };
}

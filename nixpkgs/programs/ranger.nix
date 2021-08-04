
{ pkgs, ... }:

{
  home.packages = [
    pkgs.ranger
  ];

  # File preview is constantly getting the CPU stuck trying to unpack epubs or
  # pdfs. Good riddance.
  xdg.configFile."ranger/rc.conf".text = ''
    set preview_files=false
  '';
}

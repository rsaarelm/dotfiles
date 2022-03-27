# Wayland / Sway based gui environment.
{ pkgs, ... }:

{
  # Expands on cli-core, so that gets imported too.
  imports = [
    ./cli-core.nix
  ];

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;

    extraPackages = with pkgs; [
      swaylock
      swayidle
      xwayland   # Legacy apps
      waybar     # Status bar
      wl-clipboard
      mako       # Notification daemon
      alacritty  # Terminal
      wdisplays
      rofi
    ];
  };

  # Sound on
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;

    # Bluetooth support
    package = pkgs.pulseaudioFull;
  };
}

{ config, pkgs, ... }:

# Example for new systems

{
  imports = [
    ../settings.nix
    ../home-network.nix
    ../gui-core.nix
  ];

  # NETWORK

  networking.hostName = "unnanmed-minimal-config";

  system.stateVersion = "21.05";
}


{ config, pkgs, ... }:

{
  imports =
    [
      # To enable nixos-hardware channel:
      #
      # sudo nix-channel --add https://github.com/NixOS/nixos-hardware/archive/master.tar.gz nixos-hardware
      # sudo nix-channel --update
      <nixos-hardware/raspberry-pi/4>
      ./hardware-configuration.nix
      ../common.nix
    ];

  # Use the extlinux boot loader. (NixOS wants to enable GRUB by default)
  boot.loader.grub.enable = false;
  # Enables the generation of /boot/extlinux/extlinux.conf
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "neodymium"; # Define your hostname.

  networking.wireless = {
    enable = true;
    userControlled.enable = true;
    interfaces = [ "wlan0" ];
  };

  hardware.bluetooth = {
    enable = true;
    settings = { General.Enable = "Source,Sink,Media,Socket"; };
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.eth0.useDHCP = true;
  networking.interfaces.wlan0.useDHCP = true;

  hardware.raspberry-pi."4".fkms-3d.enable = true;

  services.xserver = {
    # Emulate3Buttons won't work without this.
    libinput.enable = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}


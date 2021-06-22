# Small set of core CLI packages that it should be okay to have on any box.
{ pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs;
      let neovim = pkgs.neovim.override { vimAlias = true; };
      in [
        # System
        acpi
        acpitool
        bluez
        coreutils
        exfat-utils
        fuse_exfat
        inotifyTools
        lm_sensors
        openvpn
        pciutils
        psmisc
        usbutils

        # Basic tools
        file
        git
        home-manager
        links
        neovim
        p7zip
        pwgen
        wget
      ];

    variables.EDITOR = pkgs.lib.mkOverride 0 "nvim";
  };
}

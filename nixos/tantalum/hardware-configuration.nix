# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix> ];

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "usb_storage" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/4b8af01f-d8cc-4948-8900-8322b5681b6f";
    fsType = "ext4";
  };

  boot.initrd.luks.devices."cryptroot".device =
    "/dev/disk/by-uuid/327d3359-4b95-4609-af89-aecb88cba36d";

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/0AB3-6D80";
    fsType = "vfat";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/14c452a7-bdb1-4302-9ba6-93c92f824197"; }];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}

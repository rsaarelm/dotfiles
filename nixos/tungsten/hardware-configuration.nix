# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix> ];

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ehci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" "sr_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/e7a6a3c4-3ffd-463a-a4e8-4e27cd195dd8";
    fsType = "ext4";
    options = [ "noatime" ];
  };

  boot.initrd.luks.devices."cryptroot".device =
    "/dev/disk/by-uuid/4086738e-afc7-40a3-bf68-6114c1e1c012";

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/ece83a1a-2888-40cf-b9d9-8b72b8fd80d2";
    fsType = "ext4";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/edc60911-01ae-4544-b077-cacd499f6328"; }];

  fileSystems."/media/data0" = {
    device = "/dev/disk/by-uuid/d88fc96a-743d-44c4-a1aa-52e3245a2036";
    fsType = "ext4";
  };

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}

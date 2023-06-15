{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  imports = [
    ./hardware-configuration.nix
    ../settings.nix
    ../home-network.nix
    ../gui-core.nix
    # sudo nix-channel --add https://github.com/nix-community/home-manager/archive/release-23.05.tar.gz home-manager
    <home-manager/nixos>
  ];

  # BOOT

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # NETWORK

  networking.hostName = "tantalum";

  networking.wireless = {
    enable = true;
    userControlled.enable = true;
    interfaces = [ "wlp1s0" ];
  };

  # HARDWARE

  hardware = {
    # Steam games want this.
    opengl.driSupport32Bit = true;

    bluetooth = {
      enable = true;
      settings = { General.Enable = "Source,Sink,Media,Socket"; };
    };
  };

  # SERVICES

  environment.systemPackages = with pkgs; [
    steam
    steam-run-native
  ];

  services = {
    xserver = {
      # Touchpad
      libinput = {
        enable = true;
        touchpad.disableWhileTyping = true;
      };
    };

    autorandr.enable = true;

    acpid = {
      enable = true;

      # Disable touchpad when lid is closed, otherwise it randomly emits mouse noise.
      lidEventCommands = ''
        LID_STATE=$(/run/current-system/sw/bin/awk '{print $2}' /proc/acpi/button/lid/LID/state)
        case $LID_STATE in
          closed)
            DISPLAY=:0.0 XAUTHORITY=/home/rsaarelm/.Xauthority /run/current-system/sw/bin/xinput --disable "Elan Touchpad";;
          open)
            DISPLAY=:0.0 XAUTHORITY=/home/rsaarelm/.Xauthority /run/current-system/sw/bin/xinput --enable "Elan Touchpad";;
        esac
      '';
    };
  };

  services.xserver.dpi = 144;

  system.stateVersion = "18.03";

  # Home manager
  home-manager.users.rsaarelm = { pkgs, ... }: {
    imports = [
      ../home-manager/gui-core.nix
      ../home-manager/extras.nix
      ../home-manager/programs/chromium.nix
      ../home-manager/programs/texlive.nix
      ../home-manager/programs/zathura.nix

      ../home-manager/autorandr/tantalum.nix
      ../home-manager/style/dark-theme.nix
    ];

    xdg.configFile."nvim/guifont.vim".text = ''
      Guifont Source\ Code\ Pro:h9
    '';
  }
}

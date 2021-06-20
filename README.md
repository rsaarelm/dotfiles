Dotfiles managed with [home-manager](https://github.com/rycee/home-manager)

For dotfiles repo cloned to your home directory root,

You are expected to have files named after your host already set up.
`nixpkgs/$(HOSTNAME).nix` and `nixos/$(HOSTNAME)/` (contains configuration.nix
and hardware-configuration.nix you initially get from
`nixos-generate-config`).

## Home-manager setup

    just init-home-manager

## NixOS setup

If your `/home` and /etc` are on the same partition, you can git clone this
repo to `~/dotfiles` and them `ln -s ~/dotfiles /etc/nixos`. Otherwise just do
a separate concrete git clone directly to `/etc/` and change the base name
from `dotfiles` to `nixos`. Then use `just` to generate your
`configuration.nix` file. If system hostname isn't what you want at this
point, you need to give it as argument to the just command.

    cd /etc/nixos
    just init-nixos [MY-HOSTNAME]

## Adding a versioned configuration for a new NixOS host

Grab the `configuration.nix` and `hardware-configuration.nix` that
`nixos-generate-config` creates for you when setting up the new host for the
first time. Copy them to `nixos/[HOSTNAME]/` subdirectory.

Modify `nixos/[HOSTNAME]/configuration.nix` to import `nixos/common.nix`
unless you need to set up a custom configuration for a special needs host:

    imports = [
      ./hardware-configuration.nix
      ../common.nix
    ];

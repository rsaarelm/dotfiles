Dotfiles managed with [home-manager](https://github.com/rycee/home-manager)

For dotfiles repo cloned to your home directory root,

You are expected to have files named after your host already set up.
`nixpkgs/$(HOSTNAME).nix` and `nixos/$(HOSTNAME)/` (contains configuration.nix
and hardware-configuration.nix which can be pretty much what you get from
`nixos-generate-config`).

## Home-manager setup

    just init-home-manager

## NixOS setup

Git clone or symlink dotfiles repo to /etc/nixos. If system hostname isn't
what you want at this point, you need to give it as argument to the just
command.

    cd /etc/nixos
    just init-nixos [MY-HOSTNAME]

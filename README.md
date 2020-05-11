Dotfiles managed with [home-manager](https://github.com/rycee/home-manager)

For dotfiles repo cloned to your home directory root,

Generate config file for host, you need to have `$(HOSTNAME).nix` in `nixpkgs`
directory:

    cd ~/dotfiles/nixpkgs
    ln -s $(HOSTNAME).nix home.nix   # Must have a setup file for current host

Activate home-managed configs:

    ln -s ~/dotfiles/nixpkgs ~/.local/nixpkgs
    home-manager switch

## NixOS setup

    cd ~/dotfiles/nixos
    ./generate-configuration.sh  # Builds host-specific configuration.nix
    sudo ln -s ~/dotfiles/nixos /etc/nixos

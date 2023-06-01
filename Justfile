default:
    @just --list

# Generate configuration.nix for current host.
init-nixos host=`hostname`:
    #!/usr/bin/env sh
    # Assumes dotfiles repo has been checked out at /etc/nixos.
    if [[ ! -f configuration.nix ]]; then
        if [[ ! -e nixos/{{host}}/"configuration.nix" ]] || [[ ! -e nixos/{{host}}/"hardware-configuration.nix" ]]; then
            echo "missing nixos/{{host}}/configuration.nix or nixos/{{host}}/hardware-configuration.nix" >&2
            exit 1
        fi

        echo "Activating host {{host}}."
        echo "{ ... }: { imports = [ ./wifi.nix ./nixos/{{host}}/configuration.nix ]; }" > configuration.nix
    else
        echo "configuration.nix already exists."
    fi
    if [[ ! -f wifi.nix ]]; then
        echo "Adding placeholder wifi.nix for wireless PSKs"
        cp ./nixos/wifi.nix.sample wifi.nix
    fi

# Activate home-manager setup.
init-home-manager host=`hostname`:
    #!/usr/bin/env sh
    if [[ ! -f nixpkgs/home.nix ]]; then
        if [[ ! -e nixpkgs/{{host}}.nix ]]; then
            echo "missing nixpkgs/{{host}}.nix" >&2
            exit 1
        fi

        echo "Activating host {{host}}."
        ln -s {{host}}.nix nixpkgs/home.nix
    else
        echo "nixpkgs/home.nix already exists."
    fi
    # TODO 2023-06-01 Install home.nix to ~/.config/home-manager instead of ~/.config/nixpkgs (just renaming the dir seems to break some home-manager config..?)
    if [[ ! -d ~/.config/nixpkgs ]]; then
        mkdir -p ~/.config
        ln -s `pwd`/nixpkgs ~/.config
        echo "Linking nixpkgs to ~/.config/nixpkgs"
    fi

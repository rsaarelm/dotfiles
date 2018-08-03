Dotfiles managed with GNU Stow

The dotfiles project directory should be at the toplevel home directory for
stow to work right. To install dotfiles,

    cd ~/dotfiles
    stow [package]

For a NixOS box, make /etc/nixos a symlink to ~/dotfiles/nixos and run
`ln-host` in the nixos dir with the current host's name to link the
host-specific configurations as the system configurations.

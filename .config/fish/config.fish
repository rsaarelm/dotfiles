if status is-interactive
    # Commands to run in interactive sessions can go here
    tty-colors

    zoxide init fish | source

    neofetch --disable packages
end

fish_add_path ~/bin

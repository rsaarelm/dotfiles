if status is-interactive
    # Commands to run in interactive sessions can go here
    tty-colors

    zoxide init fish | source

    fastfetch -c examples/7
end

fish_add_path ~/bin

alias ls=eza

if test -f ~/.local.fish
    source ~/.local.fish
end

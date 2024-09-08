function run-term
    if hostname = 'tungsten'
        # Tungsten has cursed GPU, run non-GPU shell
        st
    else
        alacritty
    end
end

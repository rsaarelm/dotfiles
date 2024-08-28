function tty-colors
    if test "$TERM" = "linux"
        echo -en "\e]P0181818" # Black
        echo -en "\e]P1ac4242" # Maroon
        echo -en "\e]P290a959" # Green
        echo -en "\e]P3f4bf75" # Brown
        echo -en "\e]P46a9fb5" # Navy
        echo -en "\e]P5aa759f" # Purple
        echo -en "\e]P675b5aa" # Teal
        echo -en "\e]P7d8d8d8" # Silver
        echo -en "\e]P86b6b6b" # Gray
        echo -en "\e]P9c55555" # Red
        echo -en "\e]PAaac474" # Lime
        echo -en "\e]PBfeca88" # Yellow
        echo -en "\e]PC82b8c8" # Blue
        echo -en "\e]PDc28cb8" # Fuchsia
        echo -en "\e]PE93d3c3" # Aqua
        echo -en "\e]PFf8f8f8" # White
        clear # Clear artifacts
    end
end

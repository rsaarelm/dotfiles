# Spawn a program in the background.

function sp
    eval "$argv > /dev/null 2>&1 &; disown"
end

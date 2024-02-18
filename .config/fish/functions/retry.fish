# Try a command repeatedly until it succeeds.

function retry
    set retries 100
    set delay 5
    set cmd $argv

    for i in (seq 1 $retries)
        $cmd
        if test $status -eq 0
            break
        end
        echo "Command failed, retrying in $delay seconds..." 1>&2
        sleep $delay
    end
end

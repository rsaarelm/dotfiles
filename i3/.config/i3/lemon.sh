#!/bin/sh

# lemonbar -f "Noto Sans" -f "Font Awesome"

# Zap existing bars
killall lemon.sh

panel_fifo="/tmp/i3_lemonbar_${USER}"


memused() {
    read t f <<< `grep -E 'Mem(Total|Free)' /proc/meminfo |awk '{print $2}'`
    read b c <<< `grep -E '^(Buffers|Cached)' /proc/meminfo |awk '{print $2}'`
    bc <<< "100*($t - $f - $c - $b) / $t"
}

hdd() {
    read t <<< `df -h | grep ' /\$' | awk '{print $2}'`
    read c <<< `df -h | grep ' /\$' | awk '{print $3}'`
    echo -n "$c / $t"
}

batt() {
    # TODO
    echo -n "44 %"
}

while true; do
    echo -ne "${r}"
    # Hard drive
    echo -ne "\uf0a0 $(hdd) | "
    # RAM
    echo -ne "$(memused) % mem | "
    # Battery
    echo -ne "\uf242 $(batt) | "
    echo "$(date +'%Y-%m-%d %H:%M:%S')"
    sleep 1
done

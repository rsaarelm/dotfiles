#!/bin/sh
killall -q lemonbar
python3 $(dirname $0)/bar.py | lemonbar -p -f "Noto Sans" -f "Font Awesome"

#!/bin/sh

xrdb -merge <<EOF
XTerm*BorderWidth: 0

XTerm*font: xft:Source Code Pro:size=10
XTerm*background: #111111
XTerm*foreground: #dddddd
XTerm*color0: #2e3436
XTerm*color1: #cc0000
XTerm*color2: #4e9a06
XTerm*color3: #c4a000
XTerm*color4: #3465a4
XTerm*color5: #ff00e4
XTerm*color6: #00fbff
XTerm*color7: #d3d7cf
XTerm*color8: #565654
XTerm*color9: #ee3030
XTerm*color10: #8ae234
XTerm*color11: #fce94f
XTerm*color12: #729fcf
XTerm*color13: #b292af
XTerm*color14: #a2ffff
XTerm*color15: #eeeeec

EOF

echo "Loaded dark background color palette for xterm. Start xterm to use."

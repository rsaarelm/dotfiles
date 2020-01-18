" Extra votl stuff

" Highlight WikiWord-s inside lines as links
syntax match otlWikiWord /\C\v<([0-9]*[A-Z][a-z0-9]+){2,}>/ contained containedin=OL1,OL2,OL3,OL4,OL5,OL6,OL7,OL8,OL9,otlImportant
highlight def link otlWikiWord Tag

" WikiWord as the only thing on a line gets highlighted as the wiki section
" title. (Yes, it looks like votl syntax matching gets this boilerplatey...)
highlight def otlWikiHeading ctermfg=none guifg=none cterm=bold,underline gui=bold,underline
syntax match otlWikiHeading /\C\v^\t*([0-9]*[A-Z][a-z0-9]+){2,}$/ contained containedin=OL1,OL2,OL3,OL4,OL5,OL6,OL7,OL8,OL9

" Convention: Mark important entries with trailing asterisk
highlight def otlImportant ctermfg=none guifg=none cterm=bold gui=bold
syntax match otlImportant /\C\v^\t*(\I|\w|\[).* \*$/ contained containedin=OL1,OL2,OL3,OL4,OL5,OL6,OL7,OL8,OL9

" Use the same style as with non-user preformatted text, the default underline
" is ugly.
hi link UB1 Special
hi link UB2 Special
hi link UB3 Special
hi link UB4 Special
hi link UB5 Special
hi link UB6 Special
hi link UB7 Special
hi link UB8 Special
hi link UB9 Special

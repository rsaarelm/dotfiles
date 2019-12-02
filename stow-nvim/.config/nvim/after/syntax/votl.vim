" Extra votl stuff

" Highlight WikiWord-s inside lines as links
syntax match otlWikiWord /\C\v<([0-9]*[A-Z][a-z0-9]+){2,}>/ contained containedin=OL1,OL2,OL3,OL4,OL5,OL6,OL7,OL8,OL9
highlight def link otlWikiWord Underlined

" WikiWord as the only thing on a line gets highlighted as the wiki section
" title. (Yes, it looks like votl syntax matching gets this boilerplatey...)
syntax match otlWikiHeading      /\C\v^([0-9]*[A-Z][a-z0-9]+){2,}$/ contained containedin=OL1
syntax match otlWikiHeading    /\C\v^\t([0-9]*[A-Z][a-z0-9]+){2,}$/ contained containedin=OL2
syntax match otlWikiHeading /\C\v^\t{2}([0-9]*[A-Z][a-z0-9]+){2,}$/ contained containedin=OL3
syntax match otlWikiHeading /\C\v^\t{3}([0-9]*[A-Z][a-z0-9]+){2,}$/ contained containedin=OL4
syntax match otlWikiHeading /\C\v^\t{4}([0-9]*[A-Z][a-z0-9]+){2,}$/ contained containedin=OL5
syntax match otlWikiHeading /\C\v^\t{5}([0-9]*[A-Z][a-z0-9]+){2,}$/ contained containedin=OL6
syntax match otlWikiHeading /\C\v^\t{6}([0-9]*[A-Z][a-z0-9]+){2,}$/ contained containedin=OL7
syntax match otlWikiHeading /\C\v^\t{7}([0-9]*[A-Z][a-z0-9]+){2,}$/ contained containedin=OL8
syntax match otlWikiHeading /\C\v^\t{8}([0-9]*[A-Z][a-z0-9]+){2,}$/ contained containedin=OL9
highlight def link otlWikiHeading Title

" Convention: Mark important entries with trailing asterisk
syntax match otlImportant      /\C\v^(\I|\w|\[).* \*$/ contained containedin=OL1
syntax match otlImportant    /\C\v^\t(\I|\w|\[).* \*$/ contained containedin=OL2
syntax match otlImportant /\C\v^\t{2}(\I|\w|\[).* \*$/ contained containedin=OL3
syntax match otlImportant /\C\v^\t{3}(\I|\w|\[).* \*$/ contained containedin=OL4
syntax match otlImportant /\C\v^\t{4}(\I|\w|\[).* \*$/ contained containedin=OL5
syntax match otlImportant /\C\v^\t{5}(\I|\w|\[).* \*$/ contained containedin=OL6
syntax match otlImportant /\C\v^\t{6}(\I|\w|\[).* \*$/ contained containedin=OL7
syntax match otlImportant /\C\v^\t{7}(\I|\w|\[).* \*$/ contained containedin=OL8
syntax match otlImportant /\C\v^\t{8}(\I|\w|\[).* \*$/ contained containedin=OL9
highlight def otlImportant cterm=bold

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
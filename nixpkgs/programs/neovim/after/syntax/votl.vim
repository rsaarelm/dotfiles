" Extra votl stuff

" Quoted text, not formatted
syntax match otlWikiVerbatim /\C\v`[^`]+`/ contained containedin=OL1,OL2,OL3,OL4,OL5,OL6,OL7,OL8,OL9,otlImportant
highlight def link otlWikiVerbatim String

" Highlight WikiWord-s inside lines as links
syntax match otlWikiWord /\C\v<([A-Z][a-z]+){2,}>/ contained containedin=OL1,OL2,OL3,OL4,OL5,OL6,OL7,OL8,OL9,otlImportant
" Also highlight wikiword aliases that are marked up as links
highlight def link otlWikiWord Tag

syntax match otlWikiAliasBlock /\C\v\|[A-Za-z0-9._/-]+\|/ contained containedin=OL1,OL2,OL3,OL4,OL5,OL6,OL7,OL8,OL9,otlImportant
highlight def link otlWikiAliasBlock Comment
syntax match otlWikiAlias /\C\v[A-Za-z0-9._/-]+/ contained containedin=otlWikiAliasBlock
highlight def link otlWikiAlias Tag

" attributename: Attribute Value
syntax match otlWikiAttribute /\C\v^\s*[a-z][a-z0-9-]*: .*/ contained containedin=OL1,OL2,OL3,OL4,OL5,OL6,OL7,OL8,OL9,otlImportant
highlight def link otlWikiAttribute Label

" WikiWord as the only thing on a line gets highlighted as the wiki section
" title. (Yes, it looks like votl syntax matching gets this boilerplatey...)
highlight def otlWikiHeading ctermfg=none guifg=none cterm=bold,underline gui=bold,underline
syntax match otlWikiHeading      /\C\v^([A-Z][a-z]+){2,}$/ contained containedin=OL1
syntax match otlWikiHeading    /\C\v^\t([A-Z][a-z]+){2,}$/ contained containedin=OL2
syntax match otlWikiHeading /\C\v^\t{2}([A-Z][a-z]+){2,}$/ contained containedin=OL3
syntax match otlWikiHeading /\C\v^\t{3}([A-Z][a-z]+){2,}$/ contained containedin=OL4
syntax match otlWikiHeading /\C\v^\t{4}([A-Z][a-z]+){2,}$/ contained containedin=OL5
syntax match otlWikiHeading /\C\v^\t{5}([A-Z][a-z]+){2,}$/ contained containedin=OL6
syntax match otlWikiHeading /\C\v^\t{6}([A-Z][a-z]+){2,}$/ contained containedin=OL7
syntax match otlWikiHeading /\C\v^\t{7}([A-Z][a-z]+){2,}$/ contained containedin=OL8
syntax match otlWikiHeading /\C\v^\t{8}([A-Z][a-z]+){2,}$/ contained containedin=OL9

" Also match *Topic* for directly using more flexible alias-style title
syntax match otlWikiHeading      /\C\v^(\*[A-Za-z0-9._/-]+\*)$/ contained containedin=OL1
syntax match otlWikiHeading    /\C\v^\t(\*[A-Za-z0-9._/-]+\*)$/ contained containedin=OL2
syntax match otlWikiHeading /\C\v^\t{2}(\*[A-Za-z0-9._/-]+\*)$/ contained containedin=OL3
syntax match otlWikiHeading /\C\v^\t{3}(\*[A-Za-z0-9._/-]+\*)$/ contained containedin=OL4
syntax match otlWikiHeading /\C\v^\t{4}(\*[A-Za-z0-9._/-]+\*)$/ contained containedin=OL5
syntax match otlWikiHeading /\C\v^\t{5}(\*[A-Za-z0-9._/-]+\*)$/ contained containedin=OL6
syntax match otlWikiHeading /\C\v^\t{6}(\*[A-Za-z0-9._/-]+\*)$/ contained containedin=OL7
syntax match otlWikiHeading /\C\v^\t{7}(\*[A-Za-z0-9._/-]+\*)$/ contained containedin=OL8
syntax match otlWikiHeading /\C\v^\t{8}(\*[A-Za-z0-9._/-]+\*)$/ contained containedin=OL9

syntax match otlWikiHeadingStar contained "*" containedin=otlWikiHeading
highlight def link otlWikiHeadingStar Comment

" Convention: Mark important entries with trailing asterisk
highlight def otlImportant ctermfg=none guifg=none cterm=bold gui=bold
syntax match otlImportant      /\C\v^(\I|\w|\[).* \*$/ contained containedin=OL1
syntax match otlImportant    /\C\v^\t(\I|\w|\[).* \*$/ contained containedin=OL2
syntax match otlImportant /\C\v^\t{2}(\I|\w|\[).* \*$/ contained containedin=OL3
syntax match otlImportant /\C\v^\t{3}(\I|\w|\[).* \*$/ contained containedin=OL4
syntax match otlImportant /\C\v^\t{4}(\I|\w|\[).* \*$/ contained containedin=OL5
syntax match otlImportant /\C\v^\t{5}(\I|\w|\[).* \*$/ contained containedin=OL6
syntax match otlImportant /\C\v^\t{6}(\I|\w|\[).* \*$/ contained containedin=OL7
syntax match otlImportant /\C\v^\t{7}(\I|\w|\[).* \*$/ contained containedin=OL8
syntax match otlImportant /\C\v^\t{8}(\I|\w|\[).* \*$/ contained containedin=OL9


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

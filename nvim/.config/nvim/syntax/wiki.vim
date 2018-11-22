" Vim syntax file
" Language: wiki
if exists("b:current_syntax")
  finish
endif

syn match wikiMeta '\v^#.*'
highlight def link wikiMeta Comment

syn match wikiWord '\C\v<([A-Z][a-z0-9]+){2,}>'
highlight def link wikiWord Statement

syn match wikiTitle '\C\v^([A-Z][a-z0-9]+){2,}$'
highlight def link wikiTitle Function

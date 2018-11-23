" Vim syntax file
" Language: wiki
if exists("b:current_syntax")
  finish
endif

" Metadata outside the wiki entries like date stamps if the multi-entry
" wikifile is being written chronologically.
syn match wikiMeta '\v^#.*'
highlight def link wikiMeta Comment

syn match wikiWord '\C\v<([A-Z][a-z0-9]+){2,}>'
highlight def link wikiWord Statement

syn match wikiTitle '\C\v^([A-Z][a-z0-9]+){2,}$'
highlight def link wikiTitle Function

" Markdown-style literal syntax for quoting code etc. without WikiWord-ifying
" the CamelCaps bits.
syn match wikiLiteral '`[^`]*`'
syn match wikiLiteral '^    .*$'
highlight def link wikiLiteral Constant

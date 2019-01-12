" Follow into WikiWord.otl at WikiWord
setlocal suffixesadd=.otl

" Follow wikiword file link, open new file if one doesn't exist.
" (Overriding gF instead of gf so that you can use gf as a non-file-opening
" navigator)
noremap <buffer> gF :e %:h/<cfile>.otl<CR>

" Search for local definition of word under cursor (word is only thing on
" line)
noremap <buffer> gd /^\s*<C-r>=expand("<cword>")<CR>\s*$<CR>

" Remove - from keyword so that WikiWord-s hyphenation pattern works
setlocal iskeyword-=45

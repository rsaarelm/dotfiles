" Follow into WikiWord.otl at WikiWord
setlocal suffixesadd=.otl

" Follow wikiword file link, open new file if one doesn't exist.
" (Overriding gF instead of gf so that you can use gf as a non-file-opening
" navigator)
noremap <buffer> gF :e %:h/<cfile>.otl<CR>

" Search for local definition of word under cursor (word is only thing on
" line)
noremap <buffer> gd /^\s*[(*]\?<C-r>=expand("<cword>")<CR>[)*]\?\s*$<CR>zv

" Remove ', - from keyword so that WikiWord's and WikiWord-s patterns work
setlocal iskeyword-=39
setlocal iskeyword-=45

" Don't wrap lines when cursor is on a table
autocmd CursorMoved,CursorMovedI <buffer> if IsVimOutlinerTable() | setlocal nowrap | else | setlocal wrap | endif

function! IsVimOutlinerTable()
    return getline('.') =~# '^\t*| '
endfunction

if has("conceal")
  setlocal conceallevel=2 concealcursor=nc
endif

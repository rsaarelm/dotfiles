" Description {{{
"   Original by Shai Coleman, 2008-04-21.  http://colemak.com/
"   Modified by Joel Esko, 2012-09-14.
"
"   Load colemak.vim after all other Vim scripts.
"
"   Refer to ../README.md for keymap explanations.
" }}}
" Require Vim >=7.0 {{{
    if v:version < 700 | echohl WarningMsg | echo "colemak.vim: You need Vim version 7.0 or later." | echohl None | finish | endif
" }}}
" Up/down/left/right {{{
    nnoremap n h|xnoremap n h|onoremap h h|
    nnoremap e j|xnoremap e j|onoremap n j|
    nnoremap u k|xnoremap u k|onoremap e k|
    nnoremap i l|xnoremap i l|onoremap i l|
" }}}
" Turbo navigation {{{
    " Works with counts, see ":help complex-repeat"
    nnoremap <silent> N @='5h'<CR>|xnoremap <silent> N @='5h'<CR>|onoremap <silent> N @='5h'<CR>|
    nnoremap <silent> E @='5j'<CR>|xnoremap <silent> E @='5j'<CR>|onoremap <silent> E @='5j'<CR>|
    nnoremap <silent> U @='5k'<CR>|xnoremap <silent> U @='5k'<CR>|onoremap <silent> U @='5k'<CR>|
    nnoremap <silent> I @='5l'<CR>|xnoremap <silent> I @='5l'<CR>|onoremap <silent> I @='5l'<CR>|
" }}}
" Words forward/backward {{{
    " l/L = back word/WORD
    " h/H = end of word/WORD
    " y/Y = forward word/WORD
    nnoremap l b|xnoremap l b|onoremap l b|
    nnoremap L B|xnoremap L B|onoremap L B|
    nnoremap h e|xnoremap h e|onoremap h e|
    nnoremap H E|xnoremap H E|onoremap H E|
    nnoremap y w|xnoremap y w|onoremap y w|
    nnoremap Y W|xnoremap Y W|onoremap Y W|
    cnoremap <C-L> <C-Left>
    cnoremap <C-Y> <C-Right>
" }}}
" inSert/Replace/append (T) {{{
    nnoremap s i|
    nnoremap S I|
    nnoremap t a|
    nnoremap T A|
" }}}
" Change {{{
    nnoremap w c|xnoremap w c|
    nnoremap W C|xnoremap W C|
    nnoremap ww cc|
" }}}
" Cut/copy/paste {{{
    nnoremap c y|xnoremap c y|
    nnoremap cc yy|
    nnoremap v p|xnoremap v p|
    nnoremap C yy|xnoremap C y|
    nnoremap V P|xnoremap V P|
" }}}
" Undo/redo {{{
    nnoremap z u|xnoremap z :<C-U>undo<CR>|
    nnoremap gz U|xnoremap gz :<C-U>undo<CR>|
    nnoremap Z <C-R>|xnoremap Z :<C-U>redo<CR>|
" }}}
" Visual mode {{{
    nnoremap a v|xnoremap a v|
    nnoremap A V|xnoremap A V|
    " Make insert/add work also in visual line mode like in visual block mode
    xnoremap <silent> <expr> s (mode() =~# "[V]" ? "\<C-V>0o$I" : "I")
    xnoremap <silent> <expr> S (mode() =~# "[V]" ? "\<C-V>0o$I" : "I")
    xnoremap <silent> <expr> t (mode() =~# "[V]" ? "\<C-V>0o$A" : "A")
    xnoremap <silent> <expr> T (mode() =~# "[V]" ? "\<C-V>0o$A" : "A")
" }}}
" Search {{{
    " f/F are unchanged
    nnoremap p t|xnoremap p t|onoremap p t|
    nnoremap P T|xnoremap P T|onoremap P T|
    nnoremap b ;|xnoremap b ;|onoremap b ;|
    nnoremap B ,|xnoremap B ,|onoremap B ,|
    nnoremap k n|xnoremap k n|onoremap k n|
    nnoremap K N|xnoremap K N|onoremap K N|
" }}}
" inneR text objects {{{
    " e.g. dip (delete inner paragraph) is now drp
    onoremap r i
" }}}
" Folds, etc. {{{
    nnoremap j z|xnoremap j z|
    nnoremap jn zj|xnoremap jn zj|
    nnoremap je zk|xnoremap je zk|
" }}}
" Overridden keys must be prefixed with g {{{
    nnoremap gX X|xnoremap gX X|
    nnoremap gu gk|xnoremap gu gk|
    nnoremap ge gj|xnoremap ge gj|
    nnoremap gK K|xnoremap gK K|
    nnoremap gL L|xnoremap gL L|
    nnoremap gv gp|xnoremap gv gp|
    nnoremap gV gP|xnoremap gV gP|
" }}}
" Window handling {{{
    nnoremap <C-W>n <C-W>h|xnoremap <C-W>n <C-W>h|
    nnoremap <C-W>e <C-W>j|xnoremap <C-W>e <C-W>j|
    nnoremap <C-W>u <C-W>k|xnoremap <C-W>u <C-W>k|
    nnoremap <C-W>i <C-W>l|xnoremap <C-W>i <C-W>l|
" }}}


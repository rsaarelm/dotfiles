set mouse=a

Guifont! Source\ Code\ Pro:h8
set linespace=1

" Copy selections to clipboard automatically
vmap <LeftRelease> "*ygv

" Clipboard paste in insert mode
imap <S-Insert> <C-o>"*p

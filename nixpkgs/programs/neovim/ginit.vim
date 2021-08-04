set mouse=a

Guifont! opendyslexicmono:h11
set linespace=1

" Copy selections to clipboard automatically
vmap <LeftRelease> "*ygv

" Clipboard paste in insert mode
imap <S-Insert> <C-o>"*p

runtime colorscheme.vim

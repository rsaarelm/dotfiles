set linespace=1

set mouse=a

call GuiClipboard()

" Copy selections to clipboard automatically
vmap <LeftRelease> "*ygv

" Clipboard paste in insert mode
imap <S-Insert> <C-o>"*p

runtime colorscheme.vim
runtime guifont.vim

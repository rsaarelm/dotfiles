if hostname() == 'tantalum'
    " Low-DPI laptop display
    GuiFont! EnvyCodeR\ Nerd\ Font:h16
else
    GuiFont! EnvyCodeR\ Nerd\ Font:h10
endif

set linespace=1

set mouse=a

call GuiClipboard()

" Copy selections to clipboard automatically
vmap <LeftRelease> "*ygv

" Clipboard paste in insert mode
imap <S-Insert> <C-o>"*p

"colorscheme ayu-dark

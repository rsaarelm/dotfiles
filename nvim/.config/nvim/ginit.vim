set mouse=a
colorscheme jellybeans
Guifont! GohuFont:h10

" Copy selections to clipboard automatically
vmap <LeftRelease> "*ygv

" Jellybeans-friendly ALE highlights
highlight ALEInfoSign guifg=#777733 guibg=#333333 gui=bold
highlight ALEWarningSign guifg=#777733 guibg=#333333 gui=bold
highlight ALEErrorSign guifg=#883333 guibg=#333333 gui=bold

highlight ALEError guibg=#401515
highlight ALEWarning guibg=#153030

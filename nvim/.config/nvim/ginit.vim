set mouse=a
Guifont! Hack:h10

" Copy selections to clipboard automatically
vmap <LeftRelease> "*ygv

" Jellybeans-friendly ALE highlights
highlight ALEInfoSign guifg=#777733 guibg=#333333 gui=bold
highlight ALEWarningSign guifg=#777733 guibg=#333333 gui=bold
highlight ALEErrorSign guifg=#883333 guibg=#333333 gui=bold

highlight ALEError guibg=#401515
highlight ALEWarning guibg=#153030

" Quick-switching commands for color modes
command! LightTheme colorscheme PaperColor | set background=light
command! DarkTheme colorscheme jellybeans | set background=dark
colorscheme jellybeans

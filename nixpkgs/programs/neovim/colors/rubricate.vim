" Colorscheme that should work on terminal even if background variable is set wrong.

hi clear

if &background == 'dark'
  highlight! Normal       guibg=#111111 guifg=#ffffea
  highlight! ColorColumn  guibg=#220000 guifg=fg
else " is light background
  highlight! Normal       guibg=#ffffea guifg=black
  highlight! ColorColumn  guibg=#ffdddd guifg=fg
endif

highlight! Normal                                    ctermbg=none ctermfg=none cterm=NONE
highlight! NonText      guibg=bg guifg=fg gui=NONE   ctermbg=none ctermfg=none cterm=NONE
highlight! StatusLine   guibg=bg guifg=fg gui=NONE   ctermbg=none ctermfg=none cterm=NONE
highlight! StatusLineNC guibg=bg guifg=fg gui=NONE   ctermbg=none ctermfg=none cterm=NONE
highlight! WildMenu     guibg=bg guifg=fg gui=NONE   ctermbg=none ctermfg=none cterm=NONE
highlight! VertSplit    guibg=bg guifg=fg gui=NONE   ctermbg=none ctermfg=none cterm=NONE
highlight! Folded       guibg=bg guifg=fg gui=italic ctermbg=none ctermfg=none cterm=italic
highlight! FoldColumn   guibg=bg guifg=fg            ctermbg=none ctermfg=none
highlight! Conceal      guibg=bg guifg=fg gui=NONE   ctermbg=none ctermfg=none cterm=NONE
highlight! LineNr       guibg=bg guifg=fg gui=italic ctermbg=none ctermfg=none cterm=italic
highlight! Visual       guibg=fg guifg=bg            ctermbg=none ctermfg=none cterm=reverse
highlight! CursorLine   guibg=bg guifg=fg            ctermbg=none ctermfg=none
highlight! ColorColumn                               ctermbg=none ctermfg=none cterm=reverse

highlight! Statement    guibg=bg guifg=fg      gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! Identifier   guibg=bg guifg=fg      gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! Type         guibg=bg guifg=fg      gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! PreProc      guibg=bg guifg=fg      gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! Constant     guibg=bg guifg=darkred gui=italic ctermbg=none ctermfg=darkred cterm=italic
highlight! Comment      guibg=bg guifg=gray    gui=italic ctermbg=none ctermfg=grey    cterm=italic
highlight! Special      guibg=bg guifg=none    gui=NONE   ctermbg=none ctermfg=none    cterm=NONE
highlight! SpecialKey   guibg=bg guifg=darkred gui=bold   ctermbg=none ctermfg=darkred cterm=bold
highlight! Directory    guibg=bg guifg=fg      gui=bold   ctermbg=none ctermfg=none    cterm=bold
highlight! link Title Directory
highlight! link MoreMsg Comment
highlight! link Question Comment

hi link vimFunction Identifier

let g:colors_name = "rubricate"

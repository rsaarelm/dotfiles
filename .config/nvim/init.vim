runtime core.vim

" Plugin Manager
" ================================
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif
call plug#begin('~/.config/nvim/plugged')

" Plugins
" ================================

" Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

" NerdTree
Plug 'scrooloose/nerdtree'
map <S-TAB> :NERDTreeFind<cr><c-w><c-p>
map <TAB> :call NERDTreeToggleAndFind()<cr>
function! NERDTreeToggleAndFind()
    if (exists('t:NERDTreeBufName') && bufwinnr(t:NERDTreeBufName) != -1)
        execute ':NERDTreeClose'
    else
        if filereadable(expand('%:p'))
            execute ':NERDTreeFind'
        else
            execute ':NERDTree'
        endif
        execute ':wincmd p'
    endif
endfunction
let NERDTreeMapOpenExpl = 'j'  " Enable using Colemak vertical navigation
" Don't show random cache files.
let NERDTreeIgnore = [
            \'\.git$', '\.cpcache$', '\.lsp$', '\.o$',
            \'\.pyc$', '__pycache__', '\.clj-kondo$', '\.direnv$' ]
let NERDTreeShowHidden = 1  " Show dotfiles
" Have a more manageable navigation bar for narrow spaces.
if (winwidth(0) < 120)
    let NERDTreeWinSize = 16
endif

" Fugitive for Git
Plug 'tpope/vim-fugitive'

" Table mode
Plug 'dhruvasagar/vim-table-mode'

" Fuzzy Finder
if has('win32') || has('win64')
    Plug 'junegunn/fzf', { 'dir': '~/.fzf' }
else
    Plug 'junegunn/fzf'
endif
Plug 'junegunn/fzf.vim'
" Use FZF to change directory.
command! -nargs=* -complete=dir Cd call fzf#run(fzf#wrap({'source': 'find '.(empty(<q-args>) ? '.' : <q-args>).' -type d', 'sink': 'cd'}))
nnoremap <silent> <leader>f :Files<CR>
nnoremap <silent> <leader>b :Buffers<CR>
nnoremap <silent> <leader>; :BLines<CR>
nnoremap <silent> <leader>o :BTags<CR>
nnoremap <silent> <leader>O :Tags<CR>
" .gitignore respecting fzf command
let $FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix'

" Vimoutliner
" (Use my fork to get the empty line fix in)
Plug 'rsaarelm/vimoutliner'

" Rust support
Plug 'rust-lang/rust.vim'
let g:rustfmt_command = 'rustfmt'
let g:rustfmt_options = '--edition 2021'

" Nix file format
Plug 'LnL7/vim-nix'

" Language client
Plug 'neovim/nvim-lspconfig'

" Asynchronous completion framework
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" Register preview
Plug 'junegunn/vim-peekaboo'

" todo.txt format support
Plug 'freitass/todo.txt-vim'
" XXX: It tries to file to Done.txt when this isn't set.
let g:todo_done_filename='done.txt'

Plug 'Shatur/neovim-ayu'

" Copilot
Plug 'github/copilot.vim'
Plug 'nvim-lua/plenary.nvim'
Plug 'CopilotC-Nvim/CopilotChat.nvim'

call plug#end()

lua << EOF
require("CopilotChat").setup()
EOF
nnoremap <leader>cce :CopilotChatExplain<cr>
nnoremap <leader>cct :CopilotChatTests<cr>
nnoremap <leader>ccr :CopilotChatReview<cr>
nnoremap <leader>ccR :CopilotChatRefactor<cr>
nnoremap <leader>ccv :CopilotChatToggle<cr>

" Language client setup
if has('nvim-0.7')

lua << EOF
local nvim_lsp = require('lspconfig')

local opts = { noremap=true, silent=true }
vim.api.nvim_set_keymap('n', '<space>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
vim.api.nvim_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
vim.api.nvim_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
vim.api.nvim_set_keymap('n', '<space>q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)

local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>r', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>a', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>=', '<cmd>lua vim.lsp.buf.format({async = true})<CR>', opts)

    -- https://old.reddit.com/r/rust/comments/1geyfld/rustanalyzer_server_cancelled_the_request_in/
    -- as of 2024-12-13, fix from https://github.com/neovim/neovim/issues/30985
    -- TODO: Remove the LSP cancel event shim when NeoVim has updated with the fix, prolly sometime in early 2025
    for _, method in ipairs({ 'textDocument/diagnostic', 'workspace/diagnostic' }) do
        local default_diagnostic_handler = vim.lsp.handlers[method]
        vim.lsp.handlers[method] = function(err, result, context, config)
            if err ~= nil and err.code == -32802 then
                return
            end
            return default_diagnostic_handler(err, result, context, config)
        end
    end
end

nvim_lsp.rust_analyzer.setup({
  on_attach=on_attach,
  settings = {
    ["rust-analyzer"] = {
      assist = {
        importEnforceGranularity = true,
        importGranularity = "crate",
        importPrefix = "plain",
      },
      cargo = {
        loadOutDirsFromCheck = true,
        allFeatures = true,
      },
      -- Use clippy instead of check to report errors
      checkOnSave = {
        command = "clippy",
      },
      diagnostics = {
        enable = true,
        enableExperimental = true,
      },
      procMacro = {
        enable = true,
      },
    }
  },
})
EOF

endif


" Settings
" ================================
set virtualedit=block  " Free block placement in visual mode

set ignorecase      " Default search is case-insensitive
set smartcase       " ...except when you write caps in the search expression

set nojoinspaces    " Two spaces after a period is an abomination
set colorcolumn=81  " Show the forbidden zone
set wrap            " Show long lines by default
set textwidth=78

set listchars=tab:›…,trail:·,nbsp:░  " Show trailing whitespace
set list

set lazyredraw      " Don't update screen when running macros
set hlsearch        " Highlight search results

set expandtab       " Don't use physical tabs as a rule
set softtabstop=-1  " Use shiftwidth everywhere
set shiftwidth=4
set shiftround      " Snap to shiftwidth

set noswapfile      " Don't create swap files

set guioptions=ac   " Minimize GUI cruft
set guicursor+=a:blinkon0

set hidden          " Allow unsaved stuff in background buffers

set wildmode=longest,list,full  " Don't tab complete wrong options

set formatoptions-=t " Don't word wrap by default in eg. config files

let g:netrw_silent=1 " Don't prompt for ENTER when saving over scp

set spelllang=en_us
set spellfile=~/.config/nvim/spell/en.utf-8.add

set signcolumn=no   " Hide languageclient error column

" Define some extra digraphs
digraph Nx 8469  " ℕ
digraph Qx 8474  " ℚ

" Turn off mouse support in terminal, it seems to mostly make copy-paste work
" wrong. It'll be reactivated for GUI mode in ginit.vim.
set mouse=

" Key mapping
" ================================

" Fast buffer switching
nnoremap <BS> <C-^>
nnoremap <left> :bp<cr>
nnoremap <right> :bn<cr>
" And since we're overwriting the arrow keys anyway, make up and down scrolly
nnoremap <up> <C-y>k
nnoremap <down> <C-e>j

" Exit terminal mode easily
tnoremap <S-Esc> <C-\><C-n>

" Fast macro repeat
nnoremap <F4> @@

" Quickfix error site navigation
nnoremap <silent> <M-k> :lne<CR>zv
nnoremap <silent> g<M-k> :lp<CR>zv
nnoremap <silent> <C-k> :cn<CR>zv
nnoremap <silent> g<C-k> :cp<CR>zv

" Buffer navigation hotkeys
noremap <unique> <M-1> 1gt
noremap <unique> <M-2> 2gt
noremap <unique> <M-3> 3gt
noremap <unique> <M-4> 4gt
noremap <unique> <M-5> 5gt
noremap <unique> <M-6> 6gt
noremap <unique> <M-7> 7gt
noremap <unique> <M-8> 8gt
noremap <unique> <M-9> 9gt
noremap <unique> <M-0> 10gt

" Semantic linebreaks
" https://sembr.org/

" In Sembr mode, period and question mark newline by default, and you press
" ctrl to inhibit this.
" Remap ?-key without shift (/) to ?-and-newline.
" Only there's some weirdness with mapping /,
" you need to map _ instead, https://stackoverflow.com/q/9051837
command! Sembr inoremap <buffer> . .<cr>|inoremap <buffer> <C-.> .|inoremap <buffer> ? ?<cr>|inoremap <buffer> <C-_> ?|setl fo-=tc
command! NoSembr iunmap <buffer> .|iunmap <buffer> <C-.>|iunmap <buffer> ?|iunmap <buffer> <C-_>|setl fo+=tc

" Abbreviations
" ================================

" Timestamp abbreviation
iabbr tsp <C-r>=strftime("%Y-%m-%d")<cr>
iabbr tspt <C-r>=strftime("%Y-%m-%d %H:%M")<cr>
iabbr tspz <C-r>=strftime("%Y-%m-%dT%H:%M:%S%z")<cr>

iabbrev ansired \x1b[1;31m\x1b[0m<esc>7ha
iabbrev ansigreen \x1b[1;32m\x1b[0m<esc>7ha
iabbrev ansiyellow \x1b[1;33m\x1b[0m<esc>7ha
iabbrev ansiblue \x1b[1;34m\x1b[0m<esc>7ha
iabbrev ansimagenta \x1b[1;35m\x1b[0m<esc>7ha
iabbrev ansicyan \x1b[1;36m\x1b[0m<esc>7ha

" Shebang for embedded python in notebooks
iabbrev shpy #!/usr/bin/env python3

" Filetype settings
" ================================

filetype plugin on
filetype indent on

" Bad IDM file support by just piggy-backing off votl
autocmd BufNewFile,BufRead *.idm set filetype=votl

autocmd FileType text setlocal fo+=t linebreak
autocmd FileType markdown setlocal fo-=t linebreak

autocmd FileType votl setlocal linebreak breakindentopt+=shift:3 breakindent colorcolumn=0 foldlevel=0 textwidth=80 tabstop=2 shiftwidth=2 listchars=tab:\ \ ,trail:·,nbsp:░

autocmd FileType todo setlocal linebreak breakindentopt+=shift:2 breakindent colorcolumn=0 formatoptions-=t foldexpr=getline(v:lnum)=~'^\|\\s.*'?'1':0 foldmethod=expr

" Haskell is indentation-sensitive, so set up the vertical sight-line. And the
" horizontal line too because otherwise it gets hard to see where the cursor
" is.
autocmd FileType haskell setlocal shiftwidth=2 cursorcolumn cursorline

autocmd FileType rust setlocal textwidth=78

autocmd FileType python setlocal shiftwidth=4 softtabstop=-1 formatoptions-=t colorcolumn=81

autocmd FileType go setlocal tabstop=4 listchars=tab:\ \ ,trail:· formatoptions-=t

autocmd FileType gdscript setlocal tabstop=4

autocmd FileType javascript setlocal shiftwidth=2

" Commands
" ================================

" Remove trailing whitespace.
command! WhiteClean %s/\s\+$

" Reformat Python source code using https://github.com/psf/black
command! Black !black %

function! AsciiMathAbbrevs()
    " Remove _ from set so you can write x_i and have the _i part contract
    setlocal iskeyword=@,48-57,192-255

    iabbrev <buffer> cdot ⋅
    iabbrev <buffer> circ ∘
    iabbrev <buffer> degree <BS>°
    iabbrev <buffer> dagger †
    iabbrev <buffer> oplus ⊕
    iabbrev <buffer> otimes ⊗
    iabbrev <buffer> odot ⊙
    iabbrev <buffer> sgm ∑
    iabbrev <buffer> prod ∏
    iabbrev <buffer> wedge ∧
    iabbrev <buffer> vee ∨
    iabbrev <buffer> cap ∩
    iabbrev <buffer> cup ∪
    iabbrev <buffer> int ∫
    iabbrev <buffer> oint ∮
    iabbrev <buffer> sqrt √
    iabbrev <buffer> del ∂
    iabbrev <buffer> grad ∇
    iabbrev <buffer> oo ∞
    iabbrev <buffer> emptyset ∅
    iabbrev <buffer> aleph ℵ
    iabbrev <buffer> ^-1 <BS>⁻¹
    iabbrev <buffer> ^-2 <BS>⁻²
    iabbrev <buffer> ^2 <BS>²
    iabbrev <buffer> ^3 <BS>³
    iabbrev <buffer> ^T <BS>ᵀ
    " Bit too tiny on regular terminal, in particular hard to tell _i and _1
    " apart.
    "iabbrev <buffer> _i ᵢ
    "iabbrev <buffer> _j ⱼ
    iabbrev <buffer> != ≠
    iabbrev <buffer> <= ≤
    iabbrev <buffer> >= ≥
    iabbrev <buffer> inset ∈
    iabbrev <buffer> notin ∉
    iabbrev <buffer> subs ⊂
    iabbrev <buffer> sups ⊃
    iabbrev <buffer> sube ⊆
    iabbrev <buffer> supe ⊇
    iabbrev <buffer> =- ≡
    iabbrev <buffer> ~= ≅
    iabbrev <buffer> ~~ ≈
    iabbrev <buffer> neg ¬
    iabbrev <buffer> prop ∝
    iabbrev <buffer> AA ∀
    iabbrev <buffer> EE ∃
    iabbrev <buffer> bot ⊥
    iabbrev <buffer> ttop ⊤
    iabbrev <buffer> Gamma Γ
    iabbrev <buffer> Delta Δ
    iabbrev <buffer> Theta Θ
    iabbrev <buffer> Pi Π
    iabbrev <buffer> Sigma Σ
    iabbrev <buffer> Phi Φ
    iabbrev <buffer> Psi Ψ
    iabbrev <buffer> Omega Ω
    iabbrev <buffer> alpha α
    iabbrev <buffer> beta β
    iabbrev <buffer> gamma γ
    iabbrev <buffer> delta δ
    iabbrev <buffer> epsilon ε
    iabbrev <buffer> eta η
    iabbrev <buffer> theta θ
    iabbrev <buffer> kappa κ
    iabbrev <buffer> lambda λ
    iabbrev <buffer> mu μ
    iabbrev <buffer> pi π
    iabbrev <buffer> rho ρ
    iabbrev <buffer> sigma σ
    iabbrev <buffer> tau τ
    iabbrev <buffer> phi φ
    iabbrev <buffer> psi ψ
    iabbrev <buffer> omega ω
    iabbrev <buffer> qed ∎
endfunction
command! MathAbbrev :call AsciiMathAbbrevs()

" Epilog
" ================================

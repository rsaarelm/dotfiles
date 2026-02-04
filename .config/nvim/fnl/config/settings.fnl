(set vim.opt.listchars { :tab "›…" :trail "·" :nbsp "░" })  ; Highlight iffy and misplaced whitespace
(set vim.opt.list true)

(set vim.opt.mouse "")  ; Don't try to do stuff with the mouse.
(set vim.opt.expandtab true)   ; Indent with spaces everywhere by default.
(set vim.opt.shiftwidth 4)
(set vim.opt.ignorecase true)  ; Case-insensitive search by default
(set vim.opt.smartcase true)
(set vim.opt.linebreak true)  ; Wrap at word breaks
(set vim.opt.digraph true)    ; Write digraphs with {char1} <BS> {char2}

; Define some missing digraphs
(vim.api.nvim_command "digraph Cx 8450") ; ℂ
(vim.api.nvim_command "digraph Nx 8469") ; ℕ
(vim.api.nvim_command "digraph Qx 8474") ; ℚ
(vim.api.nvim_command "digraph Rx 8477") ; ℝ
(vim.api.nvim_command "digraph Zx 8484") ; ℤ

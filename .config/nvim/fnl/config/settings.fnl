(set vim.opt.listchars { :tab "›…" :trail "·" :nbsp "░" })  ; Highlight iffy and misplaced whitespace
(set vim.opt.list true)

(set vim.opt.mouse "")  ; Don't try to do stuff with the mouse.
(set vim.opt.expandtab true)   ; Indent with spaces everywhere by default.
(set vim.opt.shiftwidth 4)
(set vim.opt.ignorecase true)  ; Case-insensitive search by default
(set vim.opt.smartcase true)
(set vim.opt.linebreak true)  ; Wrap at word breaks

; Define some missing digraphs
(vim.api.nvim_command "digraph Cb 8450") ; ℂ
(vim.api.nvim_command "digraph Nb 8469") ; ℕ
(vim.api.nvim_command "digraph Qb 8474") ; ℚ
(vim.api.nvim_command "digraph Rb 8477") ; ℝ
(vim.api.nvim_command "digraph Zb 8484") ; ℤ

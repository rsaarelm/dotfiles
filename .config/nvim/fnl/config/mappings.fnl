(set vim.g.mapleader " ")
(set vim.g.maplocalleader "\\")

; TODO: Use macros to make the keymap settings more concise.

; Bind up and down keys to N and E for Colemak layout.
; Also make their default behavior move by visual, not logical lines.
(vim.keymap.set "" "gn" "j")
(vim.keymap.set "" "n" "gj")
(vim.keymap.set "" "k" "nzv")  ; zv forces fold opening
(vim.keymap.set "" "K" "Nzv")
(vim.keymap.set "" "ge" "k")
(vim.keymap.set "" "e" "gk")
(vim.keymap.set "" "j" "e")

; Fast and Colemaky window navigation
(vim.keymap.set "" "<C-h>" "<C-w>h")
(vim.keymap.set "" "<C-l>" "<C-w>l")
(vim.keymap.set "" "<C-n>" "<C-w>j")
(vim.keymap.set "" "<C-e>" "<C-w>k")

; Default to the more useful visual selection
(vim.keymap.set "" "v" "<C-v>")
(vim.keymap.set "" "<C-v>" "v")
; XXX: Why did I have the x mode things here?
(vim.keymap.set "x" "v" "<C-v>")
(vim.keymap.set "x" "<C-v>" "v")

; Don't require shift to get command line
(vim.keymap.set "" ";" ":")

; Keep visual selection when indenting
(vim.keymap.set "v" "<" "<gv")
(vim.keymap.set "v" ">" ">gv")

; Macro repeat
(vim.keymap.set "" "<f4>" "@@")

; Timestamp abbreviations (Do these not have API?), trailing caps to make them improbable real input
(vim.cmd "iabbr tsP <C-r>=strftime(\"%Y-%m-%d\")<cr>")
(vim.cmd "iabbr tsT <C-r>=strftime(\"%Y-%m-%d %H:%M\")<cr>")
(vim.cmd "iabbr tsZ <C-r>=strftime(\"%Y-%m-%dT%H:%M:%S%z\")<cr>")

(vim.api.nvim_create_user_command
  "ClearWhitespace"
  (λ [] (local util (require :util)) (util.clear-trailing-whitespace))
  { :desc "Clear trailing whitespace from buffer" })

(vim.pack.add ["https://github.com/ibhagwan/fzf-lua"] { :confirm false })

(local fzf (require :fzf-lua))

(vim.keymap.set "n" "<Leader>f" fzf.files)
(vim.keymap.set "n" "<Leader>g" fzf.live_grep)
(vim.keymap.set "n" "<Leader>b" fzf.buffers)
(vim.keymap.set "n" "<Leader>z" fzf.zoxide)

(vim.pack.add ["https://github.com/nvim-treesitter/nvim-treesitter"] { :confirm false })

(local treesitter (require :nvim-treesitter))

(treesitter.install
  [
   :gdscript
   :rust
   ])

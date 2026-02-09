(vim.pack.add ["https://github.com/idris-community/idris2-nvim" "https://github.com/MunifTanjim/nui.nvim" "https://github.com/neovim/nvim-lspconfig"] {:confirm false})

(local idris2 (require :idris2))

(idris2.setup {})

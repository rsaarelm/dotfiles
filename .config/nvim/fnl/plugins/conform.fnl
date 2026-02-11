(vim.pack.add ["https://github.com/stevearc/conform.nvim"] {:confirm false})

(local conform (require :conform))

(conform.setup {:formatters_by_ft {:fennel [:fnlfmt]
                                   :gdscript [:gdscript-formatter]
                                   :lua [:stylua]
                                   :markdown [:rumdl]
                                   :nix [:nixfmt]
                                   :python [:ruff]
                                   :rust [:rustfmt]}})

(vim.keymap.set "" :<leader>= conform.format)

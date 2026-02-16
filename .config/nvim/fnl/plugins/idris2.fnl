(vim.pack.add ["https://github.com/idris-community/idris2-nvim"
               "https://github.com/MunifTanjim/nui.nvim"
               "https://github.com/neovim/nvim-lspconfig"]
              {:confirm false})

(local idris2 (require :idris2))

(local code-action (require :idris2.code_action))
(local hover (require :idris2.hover))
(local metavars (require :idris2.metavars))
(local repl (require :idris2.repl))

(fn custom_setup [action] ; Map the common commands with localleader
  (vim.keymap.set "" :K hover.hover {:buffer true})
  (vim.keymap.set "" :<localleader>mm metavars.request_all {:buffer true})
  (vim.keymap.set "" :<localleader>mn metavars.goto_next {:buffer true})
  (vim.keymap.set "" :<localleader>mp metavars.goto_prev {:buffer true})
  (vim.keymap.set "" :<localleader>x repl.evaluate {:buffer true})
  (vim.keymap.set "" :<localleader>a code-action.generate_def {:buffer true})
  (vim.keymap.set "" :<localleader>c code-action.case_split {:buffer true})
  (vim.keymap.set "" :<localleader>o code-action.expr_search {:buffer true})
  (vim.keymap.set "" :<localleader>d code-action.intro {:buffer true}))

(idris2.setup {:server {:on_attach custom_setup}})

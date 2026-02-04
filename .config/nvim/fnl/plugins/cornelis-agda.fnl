(vim.pack.add ["https://github.com/kana/vim-textobj-user"] {:confirm false})
(vim.pack.add ["https://github.com/neovimhaskell/nvim-hs.vim"] {:confirm false})
(vim.pack.add ["https://github.com/agda/cornelis"] {:confirm false :version "*"})

; Use the binary installed with NixOS so we don't need the huge stack build
; (set vim.g.cornelis_use_global_binary 1)

; FIXME: Running into IOTCM error (https://github.com/agda/cornelis/issues/169)
; when trying to run using the NixOS-supplied binary as of 2026-02-04, check if
; this is fixed later. Currently this is disabled and the plugin will build the
; binary from source using stack.

(vim.api.nvim_create_autocmd
  [:FileType]
  {:pattern [:agda] :callback (λ []
     (each [_ [bind cmd] (ipairs [
       ["<leader>l" ":CornelisLoad"]
       ["<leader>r" ":CornelisRefine"]
       ["<leader>d" ":CornelisMakeCase"]
       ["<leader>," ":CornelisTypeContext"]
       ["<leader>." ":CornelisTypeContextInfer"]
       ["<leader>n" ":CornelisSolve"]
       ["<leader>a" ":CornelisAuto"]
       ["gd" ":CornelisGoToDefinition"]
       ["[/" ":CornelisPrevGoal"]
       ["]/" ":CornelisNextGoal"]
       ["<C-A>" ":CornelisInc"]
       ["<C-D>" ":CornelisDec"]])]
       (vim.keymap.set "" bind (fn [] (vim.cmd cmd) {:buffer true}))))})

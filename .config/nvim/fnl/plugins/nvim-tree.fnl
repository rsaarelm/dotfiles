(vim.pack.add ["https://github.com/nvim-tree/nvim-web-devicons"] { :confirm false })
(vim.pack.add ["https://github.com/nvim-tree/nvim-tree.lua"] { :confirm false })

(local tree (require :nvim-tree))
(local api (require :nvim-tree.api))

; This should disable netrw?
(set vim.g.loaded_netrw 1)
(set vim.g.loaded_netrwPlugin 1)

(vim.keymap.set "n" "<tab>" api.tree.toggle)

(λ on-attach [bufnr]
  ; Default
  (api.config.mappings.default_on_attach bufnr)

  ; Map on this end too.
  (vim.keymap.set "n" "<tab>" api.tree.toggle { :buffer bufnr :noremap true })
  ; Remap for colemak movement.
  (vim.keymap.set "n" "e" "gk" { :buffer bufnr :noremap true }))

(tree.setup
  {
    :filters {
      :custom [
        ; Programming and VCS stuff
        "^.git$" ".o$" ".cpcache$" ".pyc$" "^.direnv$" "__pycache__$"

        ; Binary media files
        ".png$" ".wav$" ".bin$" ".blend$" ".glb$" ".jpg$"

        ; Godot stuff
        ".import$" ".res$" ".uid$"
        ]
    }

    :on_attach on-attach
  })


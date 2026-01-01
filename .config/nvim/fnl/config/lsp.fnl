(vim.diagnostic.config
  {
   :virtual_text true   ; Display the LSP code problems while editing.
   :severity_sort true  ; Display errors before warnings.
  })

(vim.lsp.config
  "clangd"
  {:cmd [ "clangd" ]
   :filetypes ["c" "cpp"]})
(vim.lsp.enable "clangd")

(vim.lsp.config
  "lua"
  {:cmd [ "lua-language-server" ]
   :filetypes ["lua"]})
(vim.lsp.enable "lua")

(vim.lsp.config
  "fennel"
  {:cmd [ "fennel-ls" ]
   :filetypes ["fennel"]
   :root_markers ["flsproject.fnl" ".git"]})
(vim.lsp.enable "fennel")

(vim.lsp.config
  "rust-analyzer"
  {:cmd [ "rust-analyzer" ]
   :filetypes ["rust"]
   :root_markers ["Cargo.toml"]})
(vim.lsp.enable "rust-analyzer")

(vim.lsp.config
  "gdscript"
  ; Connect to running Godot editor instance
  {:cmd (vim.lsp.rpc.connect "127.0.0.1" 6005)
   :filetypes ["gdscript"]
   :root_markers ["project.godot"]})
(vim.lsp.enable "gdscript")

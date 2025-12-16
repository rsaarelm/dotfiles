; We don't have from module import *, gotta name the imports.
(local { : list-plugin-modules } (require :util))

(require :config.mappings)
(require :config.settings)
(require :config.autocmds)
(require :config.lsp)

; Automatically load all plugin specs under "fnl/plugins/"
(each [_ m (pairs (list-plugin-modules))]
                    (require m))

; FIXME Copilot makes idris2 mode generate errors, keeping it unloaded until custom command is run.
(vim.api.nvim_create_user_command :CopilotActivate
                                  (fn []
                                    (vim.pack.add ["https://github.com/github/copilot.vim"]))
                                  {:desc "Load copilot plugin" :nargs "?"})

; (vim.pack.add ["https://github.com/github/copilot.vim"])
;
; ; Always start with copilot disabled
; (set vim.g.copilot_enabled false)

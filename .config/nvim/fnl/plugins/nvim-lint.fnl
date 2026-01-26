(vim.pack.add ["https://github.com/mfussenegger/nvim-lint"] {:confirm false})

(local lint (require :lint))

(vim.api.nvim_create_autocmd [:BufWritePost]
                             {:pattern ["*"] :callback (fn [] (lint.try_lint))})

(fn parse-gdscript-formatter [output bufnr]
  "Parse output from `gdscript-formatter lint`"
  (icollect [_ line (ipairs (vim.split output "\n"))]
    (if (not= line "")
        (let [[_filename line id severity msg] (vim.split line ":")]
          {: bufnr
           :lnum (- (tonumber line) 1)
           :severity (if (= severity :error)
                         vim.diagnostic.severity.ERROR
                         vim.diagnostic.severity.WARN)
           :message (string.format "%s [%s]" msg id)}))))

(set lint.linters.gdscript
     {:cmd :gdscript-formatter
      :args [:lint]
      :stdin false
      :ignore_exitcode true
      :parser parse-gdscript-formatter})

(set lint.linters_by_ft {;
                         :gdscript [:gdscript]
                         :ledger [:hledger]
                         :nix [:nix]})

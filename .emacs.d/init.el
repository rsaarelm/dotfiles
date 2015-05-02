(load "~/.emacs.d/package")         ; Initialize package system
(load "~/.emacs.d/utils")           ; Utility functions
(load "~/.emacs.d/settings")        ; Basic customizations
(load "~/.emacs.d/vim")             ; Vim emulation layer
(load "~/.emacs.d/babel")           ; Babel mode
(load "~/.emacs.d/rust")            ; Rust mode
(load "~/.emacs.d/todotxt")         ; Todo.txt mode

; Optional local settings
(if (file-exists-p "~/.emacs.d/local.el") (load "~/.emacs.d/local"))

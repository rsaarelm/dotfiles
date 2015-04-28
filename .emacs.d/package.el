(require 'package)

;; add necessary repos
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;; start the package system
(package-initialize)

;; stop emacs from starting the package system again after
;; finishing reading init.el
(setq package-enable-at-startup nil)

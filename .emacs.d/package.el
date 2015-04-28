(require 'package)

;;;; Install my desired packages if not present
;; add necessary repos
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;; start the package system
(package-initialize)

;;;; load the list of packages available (if necessary)
;;(unless package-archive-contents 
;;  (package-refresh-contents))
;;
;;;; list the packages I want
;;(setq my-package-list '(evil
;;			colemak-evil
;;			magit))
;;
;;;; install the missing packages
;;(dolist (package my-package-list)
;;  (unless (package-installed-p package)
;;    (package-install package)))
;;
;; stop emacs from starting the package system again after
;; finishing reading init.el
(setq package-enable-at-startup nil)

; dotemacs from scratch 2016-10-11

;; Package initialization
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(setq package-list
      '(better-defaults
        evil))

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'better-defaults)
(require 'evil)

;; Font
(ignore-errors (set-default-font "Dina-9"))

;; TODO: Use evil-maps.el as reference
;; Keymap
(define-key evil-motion-state-map "n" 'evil-backward-char)
(define-key evil-motion-state-map "e" 'evil-next-line)
(define-key evil-motion-state-map "i" 'evil-prevous-line)
(define-key evil-motion-state-map "o" 'evil-forward-char)


;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (evil better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

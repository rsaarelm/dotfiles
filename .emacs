; dotemacs from scratch 2016-10-11

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Font
(ignore-errors (set-default-font "Dina-9"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Use evil-maps.el as reference
;; Modify keymap for Colemak layout.
(define-key evil-normal-state-map "o" nil)
(define-key evil-normal-state-map "O" nil)
(define-key evil-normal-state-map "i" nil)
(define-key evil-normal-state-map "I" nil)
(define-key evil-normal-state-map "gi" nil)
(define-key evil-normal-state-map "k" 'evil-open-below)
(define-key evil-normal-state-map "K" 'evil-open-above)
(define-key evil-normal-state-map "l" 'evil-insert)
(define-key evil-normal-state-map "L" 'evil-insert-line)
(define-key evil-normal-state-map "gl" 'evil-insert-resume)

(define-key evil-motion-state-map "j" nil)
(define-key evil-motion-state-map "J" nil)
(define-key evil-motion-state-map "k" nil)
(define-key evil-motion-state-map "K" nil)
(define-key evil-motion-state-map "l" nil)
(define-key evil-motion-state-map "L" nil)
(define-key evil-motion-state-map "e" 'evil-next-visual-line)
(define-key evil-motion-state-map "ge" 'evil-next-line)
(define-key evil-motion-state-map "gi" 'evil-prevous-line)
(define-key evil-motion-state-map "h" 'evil-search-next)
(define-key evil-motion-state-map "H" 'evil-search-previous)
(define-key evil-motion-state-map "i" 'evil-previous-visual-line)
(define-key evil-motion-state-map "n" 'evil-backward-char)
(define-key evil-motion-state-map "o" 'evil-forward-char)

(define-key evil-window-map "\C-n" 'evil-window-new)
(define-key evil-window-map "e" 'evil-window-down)
(define-key evil-window-map "E" 'evil-window-move-very-bottom)
(define-key evil-window-map "I" 'evil-window-move-very-top)
(define-key evil-window-map "i" 'evil-window-up)
(define-key evil-window-map "k" 'evil-window-new)
(define-key evil-window-map "n" 'evil-window-left)
(define-key evil-window-map "N" 'evil-window-move-far-left)
(define-key evil-window-map "O" 'evil-window-move-far-right)
(define-key evil-window-map "o" 'evil-window-right)
(define-key evil-window-map (kbd "C-S-e") 'evil-window-move-very-bottom)
(define-key evil-window-map (kbd "C-S-i") 'evil-window-move-very-top)
(define-key evil-window-map (kbd "C-S-n") 'evil-window-move-far-left)
(define-key evil-window-map (kbd "C-S-o") 'evil-window-move-far-right)

; General ergonomics
(define-key evil-motion-state-map ";" 'evil-ex)

(evil-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Don't clobber .emacs.d with script-generated custom cruft.
(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file))

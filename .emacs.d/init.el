;; Basic settings

; Kill the GUI clutter
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)

; Don't use suspend on windowing systems, it's not the concern of Emacs there
; and doesn't even work right on tiling WMs with no concept of hiding windows.
; Instead, follow the original idea of accessing a shell and run ansi-term.
(if window-system
    (global-set-key "\C-z" 'ansi-term))

; No blinking
(blink-cursor-mode 0)

; Paste at cursor position when mouse-pasting
(setq mouse-yank-at-point t)

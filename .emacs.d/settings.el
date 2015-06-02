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

(ensure-installed 'better-defaults)

; Timestamp command for text logs
(defun insert-timestamp (x)
  "Emit a date stamp, add time if prefix argument is given"
  (interactive "P")
  (insert (format-time-string "%Y-%m-%d"))
  (if x (insert (format-time-string " %H:%M"))))

(global-set-key (kbd "C-c .") 'insert-timestamp)

; Auto linewrap in text modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 72)

(setq-default column-number-mode t)

; Do not use doubled space after a period.
(setq sentence-end-double-space nil)

(ensure-installed 'evil)
(ensure-installed 'colemak-evil)
(evil-mode 1)

; Easy window switching
(define-key evil-normal-state-map (kbd "C-n") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-e") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-u") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-i") 'evil-window-right)

; Moving left at line start moves to previous line
(setq-default evil-cross-lines t)

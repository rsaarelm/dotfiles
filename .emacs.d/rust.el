(ensure-installed 'rust-mode)
(ensure-installed 'flymake)
(ensure-installed 'flymake-rust)

; ggtags cross referencing.
(add-hook 'prog-mode-hook
      '(lambda ()
         (when (derived-mode-p 'rust-mode) 
	 (ggtags-mode 1)))) 

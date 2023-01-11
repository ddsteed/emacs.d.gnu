;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-term

(use-package multi-term)

(setq multi-term-program "/bin/zsh")
(setq system-uses-terminfo nil)

(add-to-list 'term-bind-key-alist '("C-j"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-mterm)


;;; init-mterm.el --- Summary
;;; Commentary:
;;;   multi terminal
;;; Code:

(use-package multi-term
  :ensure t)

(setq multi-term-program "/bin/zsh")
(setq system-uses-terminfo nil)

(add-to-list 'term-bind-key-alist '("C-j"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-mterm)

;;; init-mterm.el ends here

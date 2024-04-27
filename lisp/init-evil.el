;;; init-evil.el --- Summary
;;; Commentary:
;;;   vim shortcuts
;;; Code:
(use-package evil
  :ensure t)

(evil-mode 1)

;; vim-like search highlighting
(use-package evil-search-highlight-persist
  :ensure t)

; (global-evil-search-highlight-persist nil)

;; powerline
(use-package powerline)

;; j/k for browsing wrapped lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; switch
(global-set-key (kbd "C-c v") 'evil-mode); 切换到 vi mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-evil)

;;; init-evil.el ends here

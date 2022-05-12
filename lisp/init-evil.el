;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil mode
(require 'evil)
(evil-mode 1)

;; vim-like search highlighting
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

;; powerline
(require 'powerline)
(powerline-evil-vim-color-theme)
(display-time-mode t)

;; j/k for browsing wrapped lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-evil)

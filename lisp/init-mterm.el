;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-term

(use-package multi-term)

(setq multi-term-program "/bin/zsh")
(setq system-uses-terminfo nil)

(add-to-list 'term-bind-key-alist '("C-j"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-vterm
; update current directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package vterm              ;;
;;   :ensure t                     ;;
;;   :custom                       ;;
;;   (vterm-kill-buffer-on-exit t) ;;
;; )                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-mterm)


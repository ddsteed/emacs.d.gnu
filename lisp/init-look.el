;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme select
;;
;; you have to install color-theme.el, then you can M-x color-theme-select
;; to select different color theme.

;; (require 'color-theme)
;; (setq color-theme-is-global t)
;; (color-theme-initialize) 

;; My favorite themes:
;; (color-theme-lethe)
;; (color-theme-katester)  ;; most used
;; (color-theme-arjen)
;; (color-theme-billw)
;; (color-theme-black)
;; (color-theme-comidia)
;; (color-theme-comidia)
;; (color-theme-clarity)

;; (color-theme-billw)
;; (color-theme-taylor)
;; (color-theme-rotor)
;; (color-theme-hober)

;; Since emacs 24, color-theme has been replaced by deftheme. You can change the theme by loading it directly)
(load-theme 'manoj-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tranparent background
(global-set-key [(f8)] 'loop-alpha)  ;;注意这行中的F8 , 可以改成你想要的按键    
    
(setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))

(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))                ;; head value will set to
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make a frame transparent
;; (add-to-list 'default-frame-alist '(alpha . (80 70))) 
;; (modify-frame-parameters (selected-frame) '((alpha . 85)))

(set-frame-parameter (selected-frame) 'alpha '(85 . 75))
(add-to-list 'default-frame-alist '(alpha . (85 . 75)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vscode theme

(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))

;; Remove the border around the TODO word on org-mode files
(setq vscode-dark-plus-box-org-todo nil)

;; Do not set different heights for some org faces
(setq vscode-dark-plus-scale-org-faces nil)

;; Avoid inverting hl-todo face
(setq vscode-dark-plus-invert-hl-todo nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-look)

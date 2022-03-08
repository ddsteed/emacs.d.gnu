;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme select
;;
;; you have to install color-theme.el, then you can M-x color-theme-select
;; to select different color theme.

;; (require 'color-theme)
;; (setq color-theme-is-global t)
;; (color-theme-initialize) 
;; (color-theme-katester)  ; most used

;; Since emacs 24, color-theme has been replaced by deftheme. You can change the theme by loading it directly)
(load-theme 'manoj-dark t)

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
;; 自定义界面外观，一般要放在其他的theme之后，否则有些设置会被theme里重新定义

;; tranparent background
(global-set-key [(f8)] 'loop-alpha)  ; 注意这行中的F8 , 可以改成你想要的按键    
    
(setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))

(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))                
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))
    )
)

;; make a frame transparent
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 75))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 75)))

;; 如果不在X窗口下就不要menu-bar					
(if (equal window-system nil)
    (menu-bar-mode nil))

;; 把 menu 下的图标去掉
(tool-bar-mode -1)

;; 取消 scroll-bar
(set-scroll-bar-mode nil)  ; no scroll bar, even in X-Window system

;; 在标题栏显示 buffer 的名字						
(setq frame-title-format "Life is too short to be little! @ %b")
(setq user-full-name "RDS") 
									
;; 光标不要闪烁								
(blink-cursor-mode -1)		

;; 开启全局 hi lock 模式
(global-hi-lock-mode 1) 

;; 避免每次开启 hi lock mode 时询问是否需要高亮指定表达式
(setq hi-lock-file-patterns-policy #'(lambda (dummy) t)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 括号匹配高亮

(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-look)

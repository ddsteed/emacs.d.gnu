;;; init-look.el --- Summary
;;; Commentary:
;;;   Emacs looks
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme select
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; favorite theme

;; ------------
;; material theme
; (use-package material-theme
;   :ensure t)

;; ------------
;; vscode theme

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

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

; ;; ------------
; ;; timu-caribbean
; (use-package timu-caribbean-theme
;   :ensure t
;   :config
;   (load-theme 'timu-caribbean t))

;; ------------
; ;; timu-spacegrey
; (use-package timu-spacegrey-theme
;   :ensure t
;   :config
;   (load-theme 'timu-spacegrey t))

; ;; ------------
; ;; timu-rouge
; (use-package timu-rouge-theme
;   :ensure t
;   :config
;   (load-theme 'timu-rouge t))

; ;; ------------
; ;; timu-macos
; (use-package timu-macos-theme
;   :ensure t
;   :config
;   (load-theme 'timu-macos t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 自定义界面外观，一般要放在其他的 theme 之后，否则有些设置会被 theme 里重新定义

;; tranparent background
(global-set-key [(f9)] 'loop-alpha)  ; 注意这行中的 F8 , 可以改成你想要的按键
    
(setq alpha-list '((100 100) (95 75) (85 65) (75 55) (65 45)))

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
(set-frame-parameter (selected-frame) 'alpha '(85 . 65))
(add-to-list 'default-frame-alist '(alpha . (85 . 65)))

;; 如果不在 X 窗口下就不要 menu-bar					
(if (equal window-system nil)
    (menu-bar-mode nil))

;; 把 menu 下的图标去掉
(setq tool-bar-mode nil)

;; 取消 scroll-bar
(set-scroll-bar-mode nil)  ; no scroll bar, even in X-Window system

;; titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(let ((display-table (or standard-display-table (make-display-table))))
  (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│)) 
  (setq standard-display-table display-table))
(set-face-background 'vertical-border (face-background 'default))
(set-face-foreground 'vertical-border "grey")

;; 在标题栏显示 buffer 的名字						
(setq frame-title-format "Life is too short to be little! @ %b")
(setq user-full-name "RDS") 
									
;; 光标不要闪烁								
(blink-cursor-mode -1)		

;; 光标颜色
(set-cursor-color "green")

;; 光标性状
(setq-default cursor-type 'bar) 

;; 开启全局 hi lock 模式
(global-hi-lock-mode 1) 

;; 避免每次开启 hi lock mode 时询问是否需要高亮指定表达式
(setq hi-lock-file-patterns-policy #'(lambda (dummy) t)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart-mode-line: 一个让 mode line 更加漂亮、方便管理的插件，可以自动做一些模式的隐藏等等，也可以选择多种主题。
(use-package smart-mode-line
  :ensure t
  :init (sml/setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 括号匹配高亮
(use-package highlight-parentheses
  :ensure t)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package all-the-icons
  :if (display-graphic-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scrolling smoothly like vim
(setq scroll-margin 5 scroll-conservatively 9999 scroll-step 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font
;;;中文与英文字体设置

;; Setting English Font
(set-face-attribute
 'default nil
 :font "Monaco 12" 
 :height 120
)

;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
      charset
      (font-spec
         :family "PingFang SC Regular"
         :size 16
      )
   )
)

;; 缺省字体；
(setq +font-family "Iosevka Comfy")
;; modeline 字体，未设置的情况下使用 variable-pitch 字体。
(setq +modeline-font-family "Iosevka Comfy")
;; fixed-pitch 字体；
(setq +fixed-pitch-family "Iosevka Comfy")
;; variable-pitch 字体；
(setq +variable-pitch-family "LXGW WenKai Screen")

;; 中文字体；
(setq +font-unicode-family "LXGW WenKai Screen")
;; 中文字体和英文字体按照 1:1 缩放，在偶数字号的情况下可以实现等宽等高。
(setq face-font-rescale-alist '(("LXGW WenKai Screen" . 1))) ;; 1:1 缩放。
(setq +font-size 14) ;; 偶数字号。

;; 设置缺省字体。
(defun +load-base-font ()
  ;; 只为缺省字体设置 size, 其它字体都通过 :height 动态伸缩。
  (let* ((font-spec (format "%s-%d" +font-family +font-size)))
    (set-frame-parameter nil 'font font-spec)
    (add-to-list 'default-frame-alist `(font . ,font-spec))))

;; 设置各特定 face 的字体。
(defun +load-face-font (&optional frame)
  (let ((font-spec (format "%s" +font-family))
	(modeline-font-spec (format "%s" +modeline-font-family))
	(variable-pitch-font-spec (format "%s" +variable-pitch-family))
	(fixed-pitch-font-spec (format "%s" +fixed-pitch-family)))
    (set-face-attribute 'variable-pitch frame :font variable-pitch-font-spec)
    (set-face-attribute 'fixed-pitch frame :font fixed-pitch-font-spec)
    (set-face-attribute 'fixed-pitch-serif frame :font fixed-pitch-font-spec)
    (set-face-attribute 'tab-bar frame :font font-spec)
    (set-face-attribute 'mode-line frame :font modeline-font-spec)
    (set-face-attribute 'mode-line-inactive frame :font modeline-font-spec)))

;; 设置中文字体。
(defun +load-ext-font ()
  (when window-system
    (let ((font (frame-parameter nil 'font))
	  (font-spec (font-spec :family +font-unicode-family)))
      (dolist (charset '(kana han hangul cjk-misc bopomofo))
	(set-fontset-font font charset font-spec)))))

;; 设置 Emoji 和 Symbol 字体。
(defun +load-emoji-font ()
  (when window-system
    (setq use-default-font-for-symbols nil)
    (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji")) ;; Noto Color Emoji
    (set-fontset-font t 'symbol (font-spec :family "Apple Symbols")))) ;; Symbola

(add-hook 'after-make-frame-functions 
	  ( lambda (f) 
	    (+load-face-font)
	    (+load-ext-font)
	    (+load-emoji-font)))

;; 加载字体。
(defun +load-font ()
  (+load-base-font)
  (+load-face-font)
  (+load-ext-font)
  (+load-emoji-font))

(+load-font)

;; all-the-icons 只能在 GUI 模式下使用。
(when (display-graphic-p)
  (use-package all-the-icons :demand))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dashboard: 欢迎界面
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Life is too short to be little!") ;; 个性签名，随读者喜好设置
  (setq dashboard-projects-backend 'projectile) 
  (setq dashboard-startup-banner 'official) ;; 也可以自定义图片
  (setq dashboard-items '((recents  . 2)    ;; 显示多少个最近文件
                          (bookmarks . 2)   ;; 显示多少个最近书签
                          (projects . 2))   ;; 显示多少个最近项目
        )
  (dashboard-setup-startup-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-look)

;;; init-look.el ends here

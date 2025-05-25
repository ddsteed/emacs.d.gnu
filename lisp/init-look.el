;;; init-look.el --- Summary
;;; Commentary:
;;;   Emacs 外观设置
;;; Code:

(use-package doom-themes
 :ensure t
 :config
 ;; Global settings (defaults)
 (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
       doom-themes-enable-italic t) ; if nil, italics is universally disabled
;(load-theme 'doom-acario-dark t)
;(load-theme 'doom-bluloco-dark t)
;(load-theme 'doom-challenger-deep t)
 (load-theme 'doom-dark+ t)
;(load-theme 'doom-one t)

 ;; Enable flashing mode-line on errors
 (doom-themes-visual-bell-config)
 ;; Enable custom neotree theme (nerd-icons must be installed!)
 (doom-themes-neotree-config)
 ;; or for treemacs users
 (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
 (doom-themes-treemacs-config)
 ;; Corrects (and improves) org-mode's native fontification.
 (doom-themes-org-config))

  (global-set-key [(f9)] 'loop-alpha)  ; 注意这行中的 F9 , 可以改成你想要的按键
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

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(let ((display-table (or standard-display-table (make-display-table))))
    (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│)) 
    (setq standard-display-table display-table))

(set-face-background 'vertical-border (face-background 'default))
(set-face-foreground 'vertical-border "grey")

;; 在标题栏显示 buffer 的名字						
(setq frame-title-format "Life is too short to be little! @ %b")
;; 在标题栏显示 buffer 的名字						
(setq frame-title-format "Life is too short to be little! @ %b")

(setq user-full-name "RDS") 

(blink-cursor-mode -1)          ;; 光标不要闪烁		
(set-cursor-color "green")      ;; 光标颜色
(setq-default cursor-type 'bar) ;; 光标形状
  
;; 开启全局 hi lock 模式
(global-hi-lock-mode 1) 

;; 避免每次开启 hi lock mode 时询问是否需要高亮指定表达式
(setq hi-lock-file-patterns-policy #'(lambda (dummy) t)) 

;; 先把所有 mode 设置为空
(setq-default mode-line-format '(" "))

;; 行列号
(setq mode-line-number
(list
 "  (" (propertize "%l" 'face 'font-lock-type-face)
 "," (propertize "%c" 'face 'font-lock-type-face) ")   "))

;; 日期时间
(setq mode-line-datetime
    (list
     (propertize (format-time-string "%m/%d @ %1I:%M %p ") 'face 'font-lock-type-face) ))

(defun mode-line-fill (reserve)
  (when
  (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " " 'display
      `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

(defun mode-line-flush-right (right-line)
  (let ((right-length (length (format-mode-line right-line))))
    (list (mode-line-fill right-length) right-line)))

(defun truncate-mode-line-construct (construct length)
  (let* ((full-string (format-mode-line construct))
     (truncated-string (truncate-string-to-width full-string length)))
    (replace-regexp-in-string "%" "%%" truncated-string)))

(setq-default mode-line-format
      (list
       '(:eval (propertize "%e"   'face 'font-lock-type-face))
       mode-line-front-space
       '(:eval (propertize "%@"   'face 'font-lock-constant-face))
       '(:eval (propertize "%t%Z" 'face 'font-lock-string-face))
       '(:eval (propertize "%*%+" 'face 'font-lock-warning-face))
       mode-line-number
       '(:eval (propertize "  %b" 'face 'font-lock-escape-face))
       
       ;; 剩下的信息右对齐
       '(:eval (mode-line-flush-right
            (list
             mode-line-modes
             ; " ["
             ; (propertize "%F  " 'face 'font-lock-keyword-face)
             ; (propertize (truncate-mode-line-construct "%p" 3) 'face 'font-lock-comment-face)
             ; (propertize ":"    'face 'font-lock-warning-face)
             ; (propertize "%I"   'face 'font-lock-string-face)
             ; (format-time-string "%m/%d/%Y @ %1I:%M:%S %p") ; show time
             ; (format-time-string "%m/%d @ %1I:%M %p ") ; show time
             mode-line-datetime
             ; "] "
            mode-line-end-spaces)))))

; (run-with-timer 0 1 #'(lambda () (force-mode-line-update t))) ;; update time every second

;; smart-mode-line: 一个让 mode line 更加漂亮、方便管理的插件，可以自动做一些模式的隐藏等等，也可以选择多种主题。
(use-package smart-mode-line
:ensure t
:init
(setq sml/no-confirm-load-theme t)  ; avoid asking when startup
(setq sml/theme 'powerline)
(sml/setup)
:config
(setq rm-blacklist
  (format "^ \\(%s\\)$"
    (mapconcat #'identity
      '("Projectile.*" "company.*" "Google"
        "Fly*" "company-box*" "counsel*" "*ivy*"
        "Undo-Tree" "counsel" "ivy" "yas" "WK")
       "\\|"))))

(use-package highlight-parentheses
  :ensure t
)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
  
;; all-the-icons 只能在 GUI 模式下使用。
(when (display-graphic-p)
    (use-package all-the-icons
      :ensure t)
)

(setq +font-family "Iosevka Comfy")
;; modeline 字体，未设置的情况下使用 variable-pitch 字体。
(setq +modeline-font-family "Iosevka Comfy")
;; fixed-pitch 字体；
(setq +fixed-pitch-family "Iosevka Comfy")
;; variable-pitch 字体；
(setq +variable-pitch-family "LXGW WenKai Screen")

(defun +load-base-font ()
  ;; 只为缺省字体设置 size, 其它字体都通过 :height 动态伸缩。
  (let* ((font-spec (format "%s-%d" +font-family +font-size)))
    (set-frame-parameter nil 'font font-spec)
    (add-to-list 'default-frame-alist `(font . ,font-spec))))

(set-face-attribute
    'default nil
    :font "IBM Plex Sans 14" 
    :height 120
)
(when (display-graphic-p)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
            (frame-parameter nil 'font)
            charset (font-spec
                         :family "PingFang SC Regular"
                         :size 16
                    )
            )
    )
)

(setq +font-unicode-family "LXGW WenKai Screen")
;; 中文字体和英文字体按照 1:1 缩放，在偶数字号的情况下可以实现等宽等高。
(setq face-font-rescale-alist '(("LXGW WenKai Screen" . 1))) ;; 1:1 缩放。
(setq +font-size 14) ;; 偶数字号。

(defun +load-ext-font ()
  (when window-system
    (let ((font (frame-parameter nil 'font))
      (font-spec (font-spec :family +font-unicode-family)))
      (dolist (charset '(kana han hangul cjk-misc bopomofo))
    (set-fontset-font font charset font-spec)))))

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

  (defun +load-emoji-font ()
  (when window-system
    (setq use-default-font-for-symbols nil)
    (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji")) ;; Noto Color Emoji
    (set-fontset-font t 'symbol (font-spec :family "Apple Symbols"))))  ;; Symbola

(add-hook 'after-make-frame-functions 
      ( lambda (f) 
        (+load-face-font)
        (+load-ext-font)
        (+load-emoji-font)))
  
  (defun +load-font ()
  (+load-base-font)
  (+load-face-font)
  (+load-ext-font)
  (+load-emoji-font))

(+load-font)
  
(use-package dashboard
    :ensure t
    :config
    (setq dashboard-banner-logo-title "Life is too short to be little!") ;; 个性签名，随读者喜好设置
    (setq dashboard-startup-banner 'official) ;; 也可以自定义图片
    (setq dashboard-items '((recents  . 2)    ;; 显示多少个最近文件
                            (bookmarks . 2)   ;; 显示多少个最近书签
                            (projects . 2))   ;; 显示多少个最近项目
    )
    (dashboard-setup-startup-hook))

(setq native-comp-async-report-warnings-errors nil)
(when (eq system-type 'darwin) (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(display-time-mode nil)               ; status bar 不显示时间
(setq display-time-day-and-date 0)    ; 不显示时间、星期、日期
  
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

(setq-default line-spacing 5)

(setq linum-format "%4d ")

(custom-set-faces
 '(linum ((nil (:height 100)))))

(setq column-number-mode t)

(global-hl-line-mode 1)

(setq use-dialog-box nil) 

(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)

(if (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t))

(use-package good-scroll
  :ensure t
  :if window-system          ; 在图形化界面时才使用这个插件
  :init
  (good-scroll-mode))


(provide 'init-look)
;;; init-look.el ends here

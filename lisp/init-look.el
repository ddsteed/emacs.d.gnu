;;; init-look.el --- Summary
;;; Commentary:
;;;   Emacs 外观设置
;;; Code:

(use-package doom-themes
 :config
 ;; Global settings (defaults)
 (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
       doom-themes-enable-italic t) ; if nil, italics is universally disabled
;(load-theme 'doom-acario-dark t)
 (load-theme 'doom-bluloco-dark t)
;(load-theme 'doom-challenger-deep t)
;(load-theme 'doom-dark+ t)
;(load-theme 'doom-one t)
;(load-theme 'doom-palenight t)

 ;; Enable flashing mode-line on errors
 (doom-themes-visual-bell-config)

 ;; Enable custom neotree theme (nerd-icons must be installed!)
 (doom-themes-neotree-config)

 ;; or for treemacs users
 ;(setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
 (doom-themes-treemacs-config)
 ;; Corrects (and improves) org-mode's native fontification.
 (doom-themes-org-config))

(global-set-key [(f9)] 'loop-alpha)  ; 注意这行中的 F9 , 可以改成你想要的按键
(setq alpha-list '((100 100) (95 85) (90 80) (85 65) (75 55) (65 45)))

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
(set-frame-parameter (selected-frame) 'alpha '(95 . 85))
(add-to-list 'default-frame-alist '(alpha . (95 . 85)))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(let ((display-table (or standard-display-table (make-display-table))))
    (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│)) 
    (setq standard-display-table display-table))

;; 在标题栏
;(setq frame-title-format "Life is too short to be little! @ %b")
(setq frame-title-format "Life is too short to be little!")

(setq user-full-name "Hao Feng") 

;; FIXME: left-clicking a tab closes it https://github.com/ema2159/centaur-tabs/issues/225
(use-package centaur-tabs
  :demand t
  :config
  ;; FIXME: switching by numbers should probably be upstreamed
  (defun idemacs-select-nth-tab (n)
    (let* ((tabset  (centaur-tabs-current-tabset))
           (tablist (centaur-tabs-tabs tabset))
           (nth-tab (nth (- n 1) tablist)))
      (centaur-tabs-buffer-select-tab nth-tab)))
  (defmacro idemacs-generate-numeric-tab-commands ()
    (cl-loop for n from 1 to 9 collect
      `(defun ,(intern (format "idemacs-select-tab-%s" n)) ()
         (interactive)
         (idemacs-select-nth-tab ,n))
      into defuns
      finally return `(progn ,@defuns)))
  (idemacs-generate-numeric-tab-commands)
  ;; The default `centaur-tabs-buffer-groups' creates too many tab
  ;; groups, which differs from VSCode behaviour. Users may get
  ;; confused by why they can't see tabs for their files. So we reduce
  ;; the number of tab groups to just two.
  (defun centaur-tabs-buffer-groups ()
    "Return a list of groups the current buffer should belong to.

All buffer names starting with * will be grouped under \"Emacs\".
All other buffers are grouped into \"User\"."
    (list
     (cond
      ((or (string-match-p "^ ?\\*" (buffer-name))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blame-mode
                              magit-blob-mode)))
       "Emacs")
      (t "User"))))
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-show-new-tab-button t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-bar 'over
        centaur-tabs-show-count nil
        x-underline-at-descent-line t
        ;; centaur-tabs-left-edge-margin nil
  )
  (centaur-tabs-mode 1)
  :bind
  ; ("C-<prior>" . centaur-tabs-backward)
  ; ("C-<next>" . centaur-tabs-forward)
  ("M-[" . centaur-tabs-move-current-tab-to-left)
  ("M-]" . centaur-tabs-move-current-tab-to-right)
  ("M-1" . idemacs-select-tab-1)
  ("M-2" . idemacs-select-tab-2)
  ("M-3" . idemacs-select-tab-3)
  ("M-4" . idemacs-select-tab-4)
  ("M-5" . idemacs-select-tab-5)
  ("M-6" . idemacs-select-tab-6)
  ("M-7" . idemacs-select-tab-7)
  ("M-8" . idemacs-select-tab-8)
  ("M-9" . idemacs-select-tab-9)
  ("M-0" . centaur-tabs-select-end-tab))

(blink-cursor-mode -1)          ;; 光标不要闪烁		
(set-cursor-color "orange")     ;; 光标颜色
(setq-default cursor-type 'bar) ;; 光标形状

;; headline breadcrumbs
;; FIXME: show icons in crumb header line
;; FIXME: multiline crumbs
(use-package breadcrumb
  :demand t
  :config (breadcrumb-mode)
)

;; click to highlight all occurrences of a symbol
(use-package idle-highlight-mode
  :demand t
  :config
  (setq idle-highlight-idle-time 0.5
        ;; highlight subwords too
        idle-highlight-exceptions-syntax nil)
  (idle-highlight-global-mode 1)
)

(use-package highlight-indent-guides
  :commands highlight-indent-guides-mode
  :hook
  (prog-mode . highlight-indent-guides-mode)
  (text-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-auto-odd-face-perc 100
        highlight-indent-guides-auto-even-face-perc 100
        highlight-indent-guides-auto-character-face-perc 100
        highlight-indent-guides-method 'bitmap
        highlight-indent-guides-bitmap-function #'highlight-indent-guides--bitmap-line)
)

;; clickable buttons to fold/collapse headings (in a column, after line numbers)
;; https://emacs.stackexchange.com/questions/112/actionable-code-folding-in-emacs-fringe
(unless (package-installed-p 'hideshowvis)
  (package-vc-install
   '(hideshowvis :vc-backend Git :url "https://github.com/emacsmirror/hideshowvis"))
)

(add-hook 'prog-mode-hook #'hideshowvis-enable)

(setq window-divider-default-right-width 0)
(setq window-divider-default-bottom-width 0)
(setq window-divider-default-places t)  
(window-divider-mode 1)

(set-face-attribute 'fringe nil
                  :foreground (face-foreground 'default)
                  :background (face-background 'default))

(set-face-background 'vertical-border "gray")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

(defun my-change-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table))
)

(add-hook 'window-configuration-change-hook 'my-change-window-divider)

;; 开启全局 hi lock 模式
(global-hi-lock-mode 1) 

;; 避免每次开启 hi lock mode 时询问是否需要高亮指定表达式
(setq hi-lock-file-patterns-policy #'(lambda (dummy) t)) 

(global-visual-line-mode 1)

;; 先把所有 mode 设置为空
(setq-default mode-line-format '(" "))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15))
)

;; smart-mode-line: 一个让 mode line 更加漂亮、方便管理的插件，可以自动做一些模式的隐藏等等，也可以选择多种主题。
;; 这里用于屏蔽某些显示内容
(use-package smart-mode-line
  :defer t
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
         "\\|")) 
  )
)

(use-package smart-mode-line-powerline-theme
 :after (powerline smart-mode-line)
 :config
  (sml/setup)
  (sml/apply-theme 'powerline)
)

(require 'powerline)
(powerline-default-theme)

(defface my-g-face `((t (:foreground "green")))  "Green highlight")
(defface my-b-face `((t (:foreground "LightSkyBlue")))  "LightSkyBlue highlight")

;; 行列号
(setq mode-line-number
(list
 "  (" (propertize "%l" 'face 'my-g-face)
 "," (propertize "%c %p" 'face 'my-g-face) ")   "))

;; 日期时间
;; eval 的作用是执行后面的语句，否则时间就只会停留在启动时，不会动态更新。
(setq display-time-24hr-format t)
(setq mode-line-datetime
    (list
     '(:eval (propertize (format-time-string "%H:%M %m/%d %p ") 'face 'my-b-face) )
    )
)

(defun mode-line-fill (reserve)
  (when
  (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " " 'display
      `((space :align-to (- (+ right right-fringe right-margin) ,reserve))))
)

(defun mode-line-flush-right (right-line)
  (let ((right-length (length (format-mode-line right-line))))
    (list (mode-line-fill right-length) right-line))
)

(defun truncate-mode-line-construct (construct length)
  (let* ((full-string (format-mode-line construct))
     (truncated-string (truncate-string-to-width full-string length)))
    (replace-regexp-in-string "%" "%%" truncated-string))
)

(setq-default mode-line-format
      (list
       '(:eval (propertize "%e"   'face 'font-lock-type-face))
       mode-line-front-space
       '(:eval (propertize "%@"   'face 'font-lock-constant-face))
       '(:eval (propertize "%t%Z" 'face 'font-lock-string-face))
       '(:eval (propertize "%*%+" 'face 'font-lock-warning-face))
       mode-line-number
       '(:eval (propertize "  %b" 'face 'font-lock-string-face)
      )
       
       ;; 剩下的信息右对齐
       '(:eval (mode-line-flush-right
            (list
             mode-line-modes
             mode-line-datetime
             mode-line-end-spaces))
        )
      )
)
; (run-with-timer 0 1 #'(lambda () (force-mode-line-update t))) ;; update time every second

(use-package highlight-parentheses
  :defer t
)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t))
)
(global-highlight-parentheses-mode t)

(setq +font-family "Iosevka Comfy")

;; modeline 字体，未设置的情况下使用 variable-pitch 字体。
(setq +modeline-font-family "Iosevka Comfy")

;; fixed-pitch 字体；
;(setq +fixed-pitch-family "Iosevka Comfy")
(setq +fixed-pitch-family "Fira Code Retina")

;; variable-pitch 字体；
(setq +variable-pitch-family "LXGW WenKai Screen")

(defun +load-base-font ()
  ;; 只为缺省字体设置 size, 其它字体都通过 :height 动态伸缩。
  (let* ((font-spec (format "%s-%d" +font-family +font-size)))
    (set-frame-parameter nil 'font font-spec)
    (add-to-list 'default-frame-alist `(font . ,font-spec)))
)

(set-face-attribute
    'default nil
    ;:font "IBM Plex Sans 14" 
    :font "Fira Code Retina"
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
    (set-fontset-font font charset font-spec))))
)

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
    (set-face-attribute 'mode-line-inactive frame :font modeline-font-spec))
)

(defun +load-emoji-font ()
 (when window-system
   (setq use-default-font-for-symbols nil)
   (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji")) ;; Noto Color Emoji
   (set-fontset-font t 'symbol (font-spec :family "Apple Symbols")))   ;; Symbola
)

(add-hook 'after-make-frame-functions 
      ( lambda (f) 
        (+load-face-font)
        (+load-ext-font)
        (+load-emoji-font))
)

;; font insanity for Claudemacs
;;
(defun my/setup-custom-font-fallbacks-mac ()
  (interactive)
  "Configure font fallbacks on mac for symbols and emojis.
   This will need to be called every time you change your font size,
   to load the new symbol and emoji fonts."

  (setq use-default-font-for-symbols nil)

  ;; --- Configure for 'symbol' script ---
  ;; We add fonts one by one. Since we use 'prepend',
  ;; the last one added here will be the first one Emacs tries.
  ;; So, list them in reverse order of your preference.

  ;; Least preferred among this list for symbols (will be at the end of our preferred list)
  (set-fontset-font t 'symbol "Hiragino Sans" nil 'prepend)
  (set-fontset-font t 'symbol "STIX Two Math" nil 'prepend)
  (set-fontset-font t 'symbol "Zapf Dingbats" nil 'prepend)
  (set-fontset-font t 'symbol "Monaco" nil 'prepend)
  (set-fontset-font t 'symbol "Menlo" nil 'prepend)
  ;; Most preferred for symbols -- use your main font here
  (set-fontset-font t 'symbol "JetBrainsMono Nerd Font Mono" nil 'prepend)


  ;; --- Configure for 'emoji' script ---
  ;; Add fonts one by one, in reverse order of preference.

  ;; Least preferred among this list for emojis
  (set-fontset-font t 'emoji "Hiragino Sans" nil 'prepend)
  (set-fontset-font t 'emoji "STIX Two Math" nil 'prepend)
  (set-fontset-font t 'emoji "Zapf Dingbats" nil 'prepend)
  (set-fontset-font t 'emoji "Monaco" nil 'prepend)
  (set-fontset-font t 'emoji "Menlo" nil 'prepend)
  ;; (set-fontset-font t 'emoji "Noto Emoji" nil 'prepend) ;; If you install Noto Emoji
  ;; Most preferred for emojis -- use your main font here
  (set-fontset-font t 'emoji "JetBrainsMono Nerd Font Mono" nil 'prepend)
)
  
;; to test if you have a font family installed:
;   (find-font (font-spec :family "Menlo"))

;; Then, add the fonts after your setup is complete:
(add-hook 'emacs-startup-hook
          (lambda ()
            (progn
              (when (string-equal system-type "darwin")
                (my/setup-custom-font-fallbacks-mac))))
)

(defun +load-font ()
  (+load-base-font)
  (+load-face-font)
  (+load-ext-font)
  (+load-emoji-font)
)

(+load-font)

;; all-the-icons 只能在 GUI 模式下使用。
(use-package all-the-icons
  :defer t
  :when (display-graphic-p)
  :commands all-the-icons-install-fonts  ;; 安装完成之后需要执行 all-the-icons-install-fonts 命令安装对应字体
)

(use-package all-the-icons-dired
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode)
)

(use-package all-the-icons-completion
  :defer t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode)
)

(use-package nerd-icons
  :defer t
;; :custom
;; The Nerd Font you want to use in GUI
;; "Symbols Nerd Font Mono" is the default and is recommended
;; but you can use any other Nerd Font if you want
;; (nerd-icons-font-family "Symbols Nerd Font Mono")
)

(add-to-list 'nerd-icons-extension-icon-alist '("epub" nerd-icons-faicon "nf-fa-book" :face nerd-icons-green))

(use-package dashboard
    :defer t
    :config
    (setq dashboard-banner-logo-title "Life is too short to be little!") ;; 个性签名，随读者喜好设置
    (setq dashboard-startup-banner 'official) ;; 也可以自定义图片
    (setq dashboard-items '((recents   . 6)   ;; 显示多少个最近文件
                            (bookmarks . 2)   ;; 显示多少个最近书签
                            (projects  . 2))  ;; 显示多少个最近项目
    )
    (dashboard-setup-startup-hook)
)

(setq native-comp-async-report-warnings-errors nil)
(when (eq system-type 'darwin) (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil)
)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(display-time-mode nil)               ; status bar 不显示时间
(setq display-time-day-and-date 0)    ; 不显示时间、星期、日期

(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt
)

(setq-default line-spacing 5)

(setq linum-format "%4d ")

(custom-set-faces
 '(linum ((nil (:height 100)))))

(setq column-number-mode t)

(setq-default fill-column 80)

(global-hl-line-mode 1)

(setq use-dialog-box nil) 

(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)

(if (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)
)

(use-package good-scroll
  :defer t
  :if window-system          ; 在图形化界面时才使用这个插件
  :init
  (good-scroll-mode)
)


(provide 'init-look)
;;; init-look.el ends here

;;; init-global --- Summary
;;; Commentary:
;;;    Global variables

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; 取消万恶的 tab 对齐方式，否则用 emacs 以外的其它编辑器无法正确显示空白
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; if wrap lines at word boundary.
;; NOTE: this configure is global and more preferred than truncate lines.
(global-visual-line-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; general variables
(display-time-mode nil)               ; status bar 不显示时间
; (setq display-time-day-and-date 0)  ;不显示时间、星期、日期

(size-indication-mode t)
(tool-bar-mode nil)
(tooltip-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; key bindings for Hao Feng (RDS)
(global-set-key [(meta left)]  'backward-sexp)
(global-set-key [(meta right)] 'forward-sexp)

;;
(global-set-key (kbd "s-a") 'mark-whole-buffer) ; 对应 Windows 上面的 Ctrl-a 全选
(global-set-key (kbd "s-c") 'kill-ring-save)    ; 对应 Windows 上面的 Ctrl-c 复制
(global-set-key (kbd "s-s") 'save-buffer)       ; 对应 Windows 上面的 Ctrl-s 保存
(global-set-key (kbd "s-v") 'yank)              ; 对应 Windows 上面的 Ctrl-v 粘贴
(global-set-key (kbd "s-z") 'undo)              ; 对应 Windows 上面的 Ctrl-z 撤销
(global-set-key (kbd "s-x") 'kill-region)       ; 对应 Windows 上面的 Ctrl-x 剪切

(global-set-key (kbd "C-\\") 'set-mark-command) ; 重新绑定设定块标记的命令
;;(global-set-key (kbd "C-c n") 'rename-buffer) ; 重新定义更换 buffer 名字的命令
(global-set-key (kbd "C-x C-n") 'other-window)  ; 移到下一个窗口
(global-set-key (kbd "C-x C-p")
                'other-window-backward)         ; 移到上一个窗口
(global-set-key (kbd "C-x C-;") 'comment-line)  ; 注释一行

(global-set-key (kbd "C-c m") 'multi-term)      ; 启动 multi-term
;(global-set-key (kbd "C-c m") 'multi-vterm)    ; 启动 multi-vterm
(global-set-key (kbd "C-c s") 'shell)           ; 启动 shell
(global-set-key (kbd "C-c e") 'eshell)          ; 启动 eshell
(global-set-key (kbd "C-c t")
                'toggle-truncate-lines)         ; truncate-lin

(global-set-key (kbd "C-c C-x C-l")
                'display-line-numbers-mode)     ; Show line numbers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 我经常会删除多个连续的空格，可 Emacs 没有提供这个功能，我只好自己写了一
;; 个，这可是我写的第一个 elisp 哟 :-)
(defun delete-blank-chars ()
  "Delete multiple blanks."
  (interactive)
  (while (or (looking-at " ")		; 如果是空格
             (looking-at "\n"))		; 如果是回车符
    (delete-char 1)))

(global-set-key (kbd "C-x C-a")
                'delete-blank-chars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 将反悔步长取长点，即可以反悔到很久以前的情况。默认值是 30，我觉得太少了
(setq kill-ring-max 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 如果在 shell 模式下需要输入密码，为了安全性设置密码显示为 *
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)
									
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 不要总是没完没了地问 yes 或 no，直接用 y/n
(fset 'yes-or-no-p 'y-or-n-p)
									
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 递增地搜索单词
(setq search-whitespace-regexp "[ \t\r\n]+")				

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 设置行间距
(setq-default line-spacing 5)

;; 设置行号宽度
(setq linum-format "%4d ")

;; 设置行号的大小
(custom-set-faces
 '(linum ((nil (:height 100)))))

;; 设置列号
(setq column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set auto save
(setq-default auto-save-timeout 15)   ; 15 秒无动作,自动保存
(setq-default auto-save-interval 100) ; 100 个字符间隔, 自动保存

;; close backup files
(setq make-backup-files nil)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 交换两个 buffer 的内容
(defun switch-buffers ()
"Switch the two buffers in current window and next window."
(interactive)
(if (one-window-p)
    (message "There is only one window!")
  (let ((buffer1 (current-buffer))
        (buffer2 (window-buffer (next-window))))
    (switch-to-buffer buffer2)
    (set-window-buffer (next-window) buffer1))))

(define-key global-map (kbd "\C-c b") 'switch-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 用 ibuffer 帮助分析 buffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; ibuffer 里取消自动折行
(add-hook 'ibuffer-mode-hook (lambda ()
                               (visual-line-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; merge the Emacs kill-ring with the clipboard
(setq select-enable-clipboard t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Automatically save and restore sessions
(setq desktop-dirname             "~/.emacs.d/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)
(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; 增强 minibuffer 补全：vertico 和 Orderless
(package-install 'vertico)
(vertico-mode t)

(package-install 'orderless)
(setq completion-styles '(orderless))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 配置 Marginalia 增强 minubuffer 的 annotation
(package-install 'marginalia)
(marginalia-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minibuffer action 和自适应的 context menu：Embark
(package-install 'embark)
(global-set-key (kbd "C-;") 'embark-act)
(setq prefix-help-command 'embark-prefix-help-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 增强文件内搜索和跳转函数定义：Consult
(use-package consult)
(global-set-key (kbd "C-s") 'consult-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 高亮当前行，当文本内容很多时可以很容易找到光标的位置。
(global-hl-line-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 禁止显示那些“需要鼠标点击的”弹窗，改为使用 y/n 按键操作
(setq use-dialog-box nil) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open files in dired mode using 'open'
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "z")
       (lambda () (interactive)
         (let ((fn (dired-get-file-for-visit)))
           (start-process "default-app" nil "open" fn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 显示自动截断
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 用系统默认程序打开文件
(defun rds-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-01-26"
  (interactive)
  (let* (
         (-file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (-do-it-p (if (<= (length -file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))

    (when -do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (fPath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) -file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  -file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) -file-list))))))

(global-set-key (kbd "C-c o") 'rds-open-in-external-app)          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

; disable the feature that lets Ido retrieve files from subdirectories for good
(setq ido-auto-merge-work-directories-length -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open files with system default application
(use-package crux
  :bind (("C-c o" . crux-open-with)
         ("C-a" . crux-move-beginning-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 不在新 frame 打开文件（如 Finder 的 "Open with Emacs") 。
(setq ns-pop-up-frames nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 像素平滑滚动。
(if (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs 中文输入
(use-package rime
  :custom
  (default-input-method "rime"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell-check
(require 'flyspell)
(setq flyspell-issue-message-flag nil
      ispell-local-dictionary "en_GB"
      ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy, a generic completion mechanism for Emacs
;; Counsel, a collection of Ivy-enhanced versions of common Emacs commands
;; Swiper, an Ivy-enhanced alternative to Isearch
(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring); 在某些终端上 C-x C-SPC 会被映射为 C-x C-@，比如在 macOS 上，所以要手动设置
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; amx: 记录我们每次调用 M-x 时输入的命令历史，然后每次将最常用的显示在前面
(use-package amx
  :ensure t
  :init (amx-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-window: 对 C-x o 重新绑定，使用时可以为每个 window 编个号，用编号进行跳转
(use-package ace-window
  :ensure t
  :bind
  (("C-x o" . 'ace-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; good-scroll 平滑滚动
(use-package good-scroll
  :ensure t
  :if window-system          ; 在图形化界面时才使用这个插件
  :init
  (good-scroll-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key: 提示快捷键
(use-package which-key
  :ensure t
  :init
  :config
  ;; 通过 C-h 或 ? 才显示 which-key buffer
  (setopt which-key-show-early-on-C-h t)
  ;; 仅通过 C-h 触发
; (setopt which-key-idle-delay 10000)
  ;; 在随后的按键中迅速响应
  (setopt which-key-idle-secondary-delay 0.05)
  ;; 启动全局 which-key-mode
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avy: 在不操作光标的情况下，快速对文本进行复制、剪切、粘贴，大大提高了围绕光标的操作的效率
(global-set-key (kbd "C-q") nil)
(use-package avy
  :ensure t
  :bind
  (("C-q C-SPC" . avy-goto-char-timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; marginalia: 为 Emacs minibuffer 中的选项添加注解
(use-package marginalia
  :ensure t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
			  ("M-A" . marginalia-cycle)))

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
;; 英文之间自动增加空格
(use-package pangu-spacing
  :ensure t
  :config
  (global-pangu-spacing-mode 1)
  (setq pangu-spacing-real-insert-separtor t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable tramp: it often hang up emacs :-(
(setq tramp-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Smart Input Source (SIS)
(use-package sis
  :ensure t)

(sis-ism-lazyman-config nil "rime" 'native)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-global)

;;; init-global.el ends here

;; Global variables

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; 取消万恶的tab对齐方式，否则用emacs以外的其它编辑器无法正确显示空白
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; wrap lines at word boundary
(global-visual-line-mode t)

(setq-default fill-column 200000000) ; column for paragraph filling 

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
(global-set-key (kbd "s-a") 'mark-whole-buffer) ; 对应Windows上面的 Ctrl-a 全选
(global-set-key (kbd "s-c") 'kill-ring-save)    ; 对应Windows上面的 Ctrl-c 复制
(global-set-key (kbd "s-s") 'save-buffer)       ; 对应Windows上面的 Ctrl-s 保存
(global-set-key (kbd "s-v") 'yank)              ; 对应Windows上面的 Ctrl-v 粘贴
(global-set-key (kbd "s-z") 'undo)              ; 对应Windows上面的 Ctrl-z 撤销
(global-set-key (kbd "s-x") 'kill-region)       ; 对应Windows上面的 Ctrl-x 剪切

(global-set-key (kbd "C-\\") 'set-mark-command) ; 重新绑定设定块标记的命令
(global-set-key (kbd "C-c n") 'rename-buffer)   ; 重新定义更换buffer名字的命令
(global-set-key (kbd "C-x C-n") 'other-window)  ; 移到下一个窗口
(global-set-key (kbd "C-x C-;") 'comment-line)  ; 注释一行

(global-set-key (kbd "C-c m") 'multi-term)      ; 启动 multi-term
(global-set-key (kbd "C-c s") 'shell)           ; 启动 shell
(global-set-key (kbd "C-c e") 'eshell)          ; 启动 eshell
(global-set-key (kbd "C-c t")
                'toggle-truncate-lines)         ; truncate-lin


(defun other-window-backward (&optional n)
  "Select the previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(global-set-key (kbd "C-x C-p") 'other-window-backward) ; 移到上一个窗口

;;
(global-set-key (kbd "C-c v") 'evil-mode); 切换到 vi mode

;;;;;;
;; 我经常会删除多个连续的空格，可Emacs没有提供这个功能，我只好自己写了一
;; 个，这可是我写的第一个elisp哟 :-) 
(defun delete-blank-chars ()
  (interactive)
  (while (or (looking-at " ")		; 如果是空格
             (looking-at "\n"))		; 如果是回车符
    (delete-char 1)))

(global-set-key "\C-x\C-a" 'delete-blank-chars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 将反悔步长取长点，即可以反悔到很久以前的情况。默认值是30，我觉得太少了
(setq kill-ring-max 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 如果在shell模式下需要输入密码，为了安全性设置密码显示为 *		
(add-hook 'comint-output-filter-functions				
          'comint-watch-for-password-prompt)				
									
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 不要总是没完没了地问yes或no，直接用y/n                               
(fset 'yes-or-no-p 'y-or-n-p)						
									
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 递增地搜索单词
(setq search-whitespace-regexp "[ \t\r\n]+")				

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 设置行号
(global-linum-mode 1) ; always show line numbers                              

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
(setq-default auto-save-timeout 15)   ; 15秒无动作,自动保存
(setq-default auto-save-interval 100) ; 100个字符间隔, 自动保存

;; close backup files
(setq make-backup-files nil)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 交换两个buffer的内容
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

; 自动截断
(add-hook 'ibuffer-mode-hook (lambda () (setq truncate-lines t)))

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
(provide 'init-global)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
(use-package org
  :pin gnu
  :hook
  (org-babel-after-execute . org-redisplay-inline-images)
  :custom
  ;; Do not ask before evaluating a code block
  (org-confirm-babel-evaluate nil)
)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 添加org插件
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 图片显示大小固定位屏幕宽度的三分之一
; (setq org-image-actual-width (/ (display-pixel-width) 3))

; 不自动设置图片的大小，请自行在org文件里指定
(setq org-image-actual-width nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
; (global-set-key  "\C-cl"  'org-store-link)
(global-set-key  "\C-ca"  'org-agenda)
(global-set-key  "\C-cc"  'org-capture)
(global-set-key  "\C-cb"  'org-switchb)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 跟踪一个特定项目的完成
(setq org-log-done 'time)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 缩进显示
(setq org-startup-indented t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 利用 highlight-regexp 高亮指定的正则表达式
(defun rds/bigboss-highlight ()
  (interactive)
  (highlight-regexp "0[0-9]\\{2\\}-[0-9]\\{8\\}" 'phone-number-lock-face)
  (highlight-regexp "Lisp\\|Scheme" 'language-lock-face)
  (highlight-regexp "神之编辑器\\|编辑器之神" 'emacs-vim-lock-face)
)

(add-hook 'org-mode-hook 'rds/bigboss-highlight)

;; org src 语法高亮
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

;; org 不同标题字体大小变化
(set-face-attribute 'org-level-1 nil :height 1.2 :bold t)
(set-face-attribute 'org-level-2 nil :height 1.1 :bold t)
(set-face-attribute 'org-level-3 nil :height 1.1)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 增加TODO的状态
; 在一个 ｜ 之内的状态属于同一个类，除了显示的标志不同外，在org内部当成同一类处理
; 字符: 该状态的快捷键
; ! :  切换到该状态时会自动添加时间戳
; @ :  切换到该状态时要求输入文字说明
; 如果同时设定@和!,使用@/!

(setq org-todo-keywords
      '((sequence "☞ TODO(t)"
                  "⚑ WAIT(w)"
                  "☟ NEXT(n)"
                  "⚔ INPR(i)"
                  "|"
                  "✰ IMPO(I)"
                  "❤ LOVE(l)"
                  "✍ NOTE(N)"
                  "✔ DONE(d)"
                  "✘ CANL(c@)"
                  "|"
                  "☕ BREK(b)"
                  )))

(setq org-todo-keyword-faces
      (quote (("☞ TODO" :foreground "magenta" :weight thin)
              ("☟ NEXT" :foreground "blue" :weight thin)
              ("⚔ INPR" :foreground "red" :weight thin)
              ("✔ DONE" :foreground "forest green" :weight thin)
              ("⚑ WAIT" :foreground "orange" :weight thin)
              ("✘ CANL" :foreground "dark red" :weight bold)
              )))


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda
; 将该目录下所有的org和org_archive文件作为日程表搜索范围
(setq org-agenda-files (directory-files-recursively "~/Work/GTD/" "\\.org*"))

;; 融合ical和agenda
(add-to-list 'org-modules 'org-mac-iCal)

; 显示节日
(setq org-agenda-include-diary t)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

(add-hook 'org-agenda-cleanup-fancy-diary-hook
          (lambda ()
            (goto-char (point-min))
            (save-excursion
              (while (re-search-forward "^[a-z]" nil t)
                (goto-char (match-beginning 0))
                (insert "0:00-24:00 ")))
            (while (re-search-forward "^ [a-z]" nil t)
              (goto-char (match-beginning 0))
              (save-excursion
                (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
              (insert (match-string 0)))))

; agenda 里面时间块彩色显示
; From: https://emacs-china.org/t/org-agenda/8679/3
(defun ljg/org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
           (background-dark-p (string= background "dark"))
           (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
;       (when (and (not (equal pos (pos-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
;               (ov (make-overlay (point-bol) (1+ (point-eol)))))
            (overlay-put ov 'face `(:background ,(car colors)
                                                :foreground
                                                ,(if background-dark-p "black" "white")))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook #'ljg/org-agenda-time-grid-spacing)

;; 日程表视图默认显示当天，可以用w，d切换称一周或一天
(setq org-agenda-span 'day)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clock

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'rds/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq rds/keep-clock-running nil)

(defun rds/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (rds/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (rds/is-project-p))
      "TODO"))))

(defun rds/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun rds/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq rds/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (rds/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (rds/clock-in-organization-task-as-default)))))

(defun rds/punch-out ()
  (interactive)
  (setq rds/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun rds/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun rds/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when rds/keep-clock-running
            (rds/clock-in-default-task)))))))

(defvar rds/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun rds/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find rds/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun rds/clock-out-maybe ()
  (when (and rds/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (rds/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'rds/clock-out-maybe 'append)

(require 'org-id)
(defun rds/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun rds/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

; global Effort estimate values
; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (closed state)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; capture
; (setq org-capture-templates '(("t" "Todo [Inbox]" entry
;                                (file+headline "~/Work/GTD/Inbox.org" "Tasks")
;                                "* TODO %i%?")
;                               ("T" "Tickler" entry
;                                (file+headline "~/Work/GTD/Tickler.org" "Tickler")
;                                "* %i%? \n %U")))

(setq org-capture-templates '(("t" "Todo [Inbox]" entry
                               (file+headline "~/Work/GTD/Inbox.org" "Tasks")
                               "* TODO %i%?")))

; add org capture file
(add-to-list 'org-capture-templates 
             '("T" "Tickler" entry (file+headline "~/Work/GTD/Tickler.org" "Tickler")
                "* %i%? \n %U"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; refile
;  Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Archive
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org bullets
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("☯" "✿" "❀" "►" "✚" "◉"))
)

;; org alert
(use-package org-alert
  :defer t
  :config
  (progn
    (setq alert-default-style 'libnotify)
  )
)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reminders

; Erase all reminders and rebuilt reminders for today from the agenda
(defun rds/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-agenda-finalize-hook 'rds/org-agenda-to-appt 'append)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
(rds/org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'rds/org-agenda-to-appt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 设置org mode自动换行
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 设置org插入图片的存储方式
(setq org-download-method 'directory)

(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(use-package org-download
	  :ensure t 
	  ;;将截屏功能绑定到快捷键：Ctrl + Shift + Y
	  :bind ("C-S-y" . org-download-screenshot)
	  :config
	  (require 'org-download)
	  ;; Drag and drop to Dired
	  (add-hook 'dired-mode-hook 'org-download-enable)
	  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 支持多个语言
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell      . t)
   (js         . t)
   (emacs-lisp . t)
   (perl       . t)
   (python     . t)   
   (C          . t)
   (fortran    . t)
   (latex      . t)
   (dot        . t)
   (css        . t)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode 转换功能
(setq org-export-backends (quote (ascii html icalendar latex md)))

;;
(require 'init-publish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 插入快捷键
(add-hook 'org-mode-hook
	  (lambda ()
	    (require 'org-tempo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ox-hugo: org to html
(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open files using system default application in org mode

(setq org-file-apps
      '(
        ("\\.xlsx\\'" . "excel %s")
        ("\\.md\\'" . "MacDown %s")
        ("\\.nb\\'" . "mathematica %s")
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org preview html
(require 'org-preview-html)

(global-set-key  "\C-cp"  'org-preview-html-mode)

;(setq org-preview-html-viewer 'xwidget)
(setq org-preview-html-viewer 'eww)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link
(global-set-key "\C-cl" 'org-insert-link)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; count words
(add-hook 'org-mode-hook 'wc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cdlatex minor-mode
(add-hook 'org-mode-hook #'turn-on-org-cdlatex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fly spell check
;; 1. hook flyspell into org-mode
(add-hook 'org-mode-hook 'flyspell-mode)
;(add-hook 'org-mode-hook 'flyspell-buffer) ; it changed org-bullets symbols

;; 2. ignore message flags
(setq flyspell-issue-message-flag nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-org)

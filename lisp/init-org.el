;;; init-org.el --- Summary
;;; Commentary:
;;;   org
;;; Code:

 (use-package org
   :pin gnu
   :hook
   (org-babel-after-execute . org-redisplay-inline-images)
   :custom
   ;; Do not ask before evaluating a code block
   (org-confirm-babel-evaluate nil)
 )

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(global-set-key  "\C-ca"  'org-agenda)
(global-set-key  "\C-cc"  'org-capture)
(global-set-key  "\C-cb"  'org-switchb)

(global-set-key "\C-cl" 'org-insert-link) ;; link

(use-package wc-mode
  :ensure t
  :defer t)

(add-hook 'org-mode-hook 'wc-mode)

;; Counting sub-headings
(cl-defun my/count-org-headings (&optional (level 4))
  "计算当前 headings 下指定 sub-headings 的数目.
LEVEL 是一个数字，作为参数提供，默认指定第 4 级"
  (interactive "nLevel: ")
  (let ((count 0))
    (save-excursion
      (org-map-entries
       (lambda ()
         (when (= (org-current-level) level)
           (setq count (+ count 1))))
       nil 'tree
       ))

    (insert (number-to-string count))
    (message "Number of level %d subheadings: %d" level count)))

(defun my/org-count-4headings-in-parentheses ()
  "Delete the content inside parentheses and execute a function."
  (interactive)
  (save-excursion
    (let ((beg (progn (backward-up-list) (point)))
          (end (progn (forward-sexp) (point))))
      (delete-region (+ beg 1) (- end 1))
      (goto-char (+ beg 1))
       (apply '(my/count-org-headings 4)))))

(add-hook 'org-mode-hook
         (lambda ()
           (local-set-key (kbd "C-c C-h n 2")
                (lambda () (interactive) (my/count-org-headings 2)))
           (local-set-key (kbd "C-c C-h n 3")
                (lambda () (interactive) (my/count-org-headings 3)))
           (local-set-key (kbd "C-c C-h n 4")
                (lambda () (interactive) (my/count-org-headings 4)))
           (local-set-key (kbd "C-c C-h c")
                'my/org-count-4headings-in-parentheses)))

(setq org-file-apps
      '(
        ("\\.xlsx\\'" . "excel %s")
        ("\\.md\\'" . "MacDown %s")
        ))

; global Effort estimate values
; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(use-package org-download
      :ensure t 
      :defer t
      ;;将截屏功能绑定到快捷键：Ctrl + Shift + Y
      :bind ("C-S-y" . org-download-screenshot)
      :config
      (require 'org-download)
      ;; Drag and drop to Dired
      (setq org-download-method 'directory)
      (add-hook 'dired-mode-hook 'org-download-enable)
      )

(add-hook 'org-mode-hook (lambda ()
                           (visual-line-mode 1)))

(setq org-image-actual-width nil)  ; 不自动设置图片的大小，请自行在 org 文件里指定
; (setq org-image-actual-width (/ (display-pixel-width) 3)) ; 图片显示大小固定位屏幕宽度的三分之一

(setq org-startup-indented t)
(org-reload)

(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

(set-face-attribute 'org-level-1 nil :height 1.2 :bold t)
(set-face-attribute 'org-level-2 nil :height 1.1 :bold t)
(set-face-attribute 'org-level-3 nil :height 1.1)

(use-package org-alert
  :ensure t
  :defer t
  :config
  (progn
    (setq alert-default-style 'libnotify)
    )
  )

(use-package org-bullets
  :ensure t
  :defer t
  :config
  (setq org-bullets-bullet-list '("☯" "✿" "❀" "►" "✚" "◉"))
  :hook (org-mode . org-bullets-mode)
  )

(setq-default org-startup-indented t
              org-pretty-entities t
              org-use-sub-superscripts "{}"
              org-hide-emphasis-markers t
              org-startup-with-inline-images t
              org-image-actual-width '(300))

(use-package org-appear
  :ensure t
  :defer t
  :hook
  (org-mode . org-appear-mode))

(use-package org-modern
  :ensure t
  :defer t
  :hook
  (org-mode . global-org-modern-mode)
  :custom
  (org-modern-keyword t)
  (org-modern-checkbox nil)
  (org-modern-table nil))

;; 将该目录下所有的 org 和 org_archive 文件作为日程表搜索范围
(setq org-agenda-files (directory-files-recursively "~/Work/GTD/" "\\.org*"))

;; 融合 ical 和 agenda
(add-to-list 'org-modules 'org-mac-iCal)

;; 显示节日
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
(defun rds/org-agenda-time-grid-spacing ()
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
                   (setq duration (org-get-at-bol 'duration)))
          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors)
                                                :foreground
                                                ,(if background-dark-p "black" "white")))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook #'rds/org-agenda-time-grid-spacing)

;; 日程表视图默认显示当天，可以用 w，d 切换称一周或一天
(setq org-agenda-span 'day)

;; remove tags from the agenda view
(customize-set-variable 'org-agenda-remove-tags t)

;; customize the prefix and keyword formats
(customize-set-variable 'org-agenda-prefix-format " %-12c %?-14t% s")
(customize-set-variable 'org-agenda-todo-keyword-format "%-10s")

;; distinct scheduling and deadlines
(customize-set-variable 'org-agenda-scheduled-leaders
                        '("[S] : " "[S] x%3d d.: "))
(customize-set-variable 'org-agenda-deadline-leaders
                        '("[D] : " "[D] +%3d d.: " "[D] -%3d d.: "))

;; refine time grid
(customize-set-variable 'org-agenda-time-grid
                        '((today require-timed remove-match)
                          (0600 1000 1200 1400 1800 2200)
                          ":  " "┈┈┈┈┈┈┈┈┈┈┈┈┈"))
(customize-set-variable 'org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈┈ now")

;; weekly overview
(add-to-list
 'org-agenda-custom-commands
 '("w" "THIS WEEK"
   ((agenda ""
            ((org-agenda-overriding-header
              (concat "THIS WEEK (W" (format-time-string "%V") ")")))))))

;; daily agenda view
(add-to-list
 'org-agenda-custom-commands
 '("d" "DAY'S AGENDA"
   ((agenda ""
            ((org-agenda-overriding-header
              (concat "TODAY (W" (format-time-string "%V") ")"))
             (org-agenda-span 'day)
             (org-agenda-sorting-strategy
              '((agenda time-up priority-down category-keep)))
             (org-agenda-show-log t)
             (org-agenda-log-mode-items '(clock)))))))

;; custom overview
(add-to-list
 'org-agenda-custom-commands
 '("c" "CUSTOM OVERVIEW"
   ((tags-todo "+PRIORITY=\"A\""
               ((org-agenda-overriding-header "PRIO A")))
    (agenda ""
            ((org-agenda-overriding-header
              (concat "TODAY (W" (format-time-string "%V") ")"))
             (org-agenda-span 'day)
             (org-agenda-sorting-strategy
              '((agenda time-up priority-down category-keep)))
             (org-agenda-show-log t)
             (org-agenda-log-mode-items '(clock))))
    (agenda ""
            ((org-agenda-overriding-header
              (concat "FOLLOWING DAYS (W" (format-time-string "%V") ")"))
             (org-agenda-skip-function
              '(org-agenda-skip-entry-if 'unscheduled))
             (org-agenda-span 6)
             (org-agenda-start-day "+1d")
             (org-agenda-start-on-weekday 1)))
    (tags-todo "+private"
               ((org-agenda-overriding-header "PRIVATE TASKS")
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'unscheduled))))
    (tags-todo "+work"
               ((org-agenda-overriding-header "WORK TASKS")
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'unscheduled))))
    (tags "CLOSED>=\"<-7d>\"|DONE>=\"<-7d>\"|CANCELLED>=\"<-7d>\""
          ((org-agenda-overriding-header "Completed in the Last 7 Days\n"))))))

    ;; Agenda log mode items to display (closed and state changes by default)
    (setq org-agenda-log-mode-items (quote (closed state)))

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(setq org-capture-templates '(("t" "Todo [Inbox]" entry
                               (file+headline "~/Work/GTD/Inbox.org" "Tasks")
                               "* TODO %i%?")))

; add org capture file
(add-to-list 'org-capture-templates 
             '("T" "Tickler" entry (file+headline "~/Work/GTD/Tickler.org" "Tickler")
                "* %i%? \n %U"))

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

;  Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(add-hook 'org-finalize-agenda-hook
          (lambda ()
            (setq appt-message-warning-time 10        ;; warn 10 min in advance
                  appt-display-diary nil              ;; do not display diary when (appt-activate) is called
                  appt-display-mode-line t            ;; show in the modeline
                  appt-display-format 'window         ;; display notification in window
                  calendar-mark-diary-entries-flag t) ;; mark diary entries in calendar
            (org-agenda-to-appt)                      ;; copy all agenda schedule to appointments
            (appt-activate 1)))                       ;; active appt (appointment notification)

(setq org-log-done 'time)

(setq org-todo-keywords
      '((sequence
         "⚔ INPR(i)"
         "☟ NEXT(n)"
         "⚑ WAIT(w!)"
         "☞ TODO(t)"
         "|"
         "✰ IMPO(I)"
         "❤ LOVE(l)"
         "✍ NOTE(N)"
         "|"
         "✔ DONE(d)"
         "✘ SUSP(s@/!)"
         "✘ CANL(c@/!)"
         "☕ BREK(b@/!)"
         )))

(setq org-todo-keyword-faces
      (quote (
              ("☞ TODO" :foreground "dark red" :weight thin)
              ("☟ NEXT" :foreground "magenta" :weight thin)
              ("⚔ INPR" :foreground "forest green" :weight thin)
              ("✔ DONE" :foreground "blue" :weight thin)
              ("⚑ WAIT" :foreground "magenta" :weight thin)
              ("⚑ SUSP" :foreground "orange" :weight thin)
              ("✘ CANL" :foreground "dark red" :weight bold)
              )))

(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-htmlize-output-type 'inline-css ;; 保留代码块高亮
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://files.cnblogs.com/csophys/orgstyle.css\"/>"
)

(use-package cdlatex
  :ensure t
  :defer t)

(add-hook 'org-mode-hook #'turn-on-org-cdlatex)

;; LaTeX previews
(use-package org-fragtog
  :ensure t
  :defer t
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-startup-with-latex-preview t)
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

;; ox-hugo: org to html
(use-package ox-hugo
  :ensure t   
  :defer t
  :after ox)

;; org preview html
(use-package org-preview-html
  :ensure t
  :defer t)

(setq org-preview-html-viewer 'eww)

(provide 'init-org)

;;; init-org.el ends here

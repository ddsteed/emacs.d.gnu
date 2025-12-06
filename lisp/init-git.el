;;; init-git.el --- Summary
;;; Commentary:
;;;   git
;;; Code:

(use-package magit
   :defer t
   :commands magit-status
   :bind
   (
    ("\C-x g m" . magit-status)
    ("\C-x g s" . magit-show-commit)
   )
   :custom
   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
   :config
   (setq ediff-diff-options "")
   (setq ediff-custom-diff-options "-u")
   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
   (setq ediff-split-window-function 'split-window-vertically)
   (setq magit-auto-revert-mode t)
)

(global-set-key (kbd "C-x M-g") 'magit-dispatch)

(use-package forge
 :defer t
 :after magit)

(use-package magit-imerge
  :defer t
)
;(global-set-key (kbd "C-x m") 'magit-show-commit)

;(use-package magit-delta
;  :hook
;  (magit-mode . magit-delta-mode)
;)

(use-package git-gutter
  :defer t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

;; 在侧边栏显示git信息
(use-package git-gutter-fringe
  :defer t
  :config
  (global-git-gutter-mode)
)

;; 显示修改信息
(use-package blamer
  :config
  (setq
        ;; 停顿多久后显示（秒）
        blamer-idle-time 0.5            ;; 0.1 is more VSCode-like, but can be glitchy in Emacs
        ;; 一行里至少留多少列再开始显示提示，避免太挤
        blamer-min-offset 40
        ;; 显示形式：overlay 表示行内显示，像注释一样
        blamer-view 'overlay 
        blamer-max-commit-message-length 80
        blamer-entire-formatter "      %s"
        blamer-pretty-time-p t
        blamer-max-lines 10)
  (global-blamer-mode -1)  ;; 默认不开启
)

(global-set-key (kbd "C-x g b") #'blamer-mode)

(use-package diff-hl
  :demand t
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

;; 只显示当前行的修改信息
(defun my/git-blame-current-line ()
  "在 minibuffer 显示当前行最后一次提交的信息（git blame）"
  (interactive)
  (unless buffer-file-name
    (user-error "当前 buffer 没有关联文件"))
  (let* ((line (line-number-at-pos))
         (line-arg (format "%d,%d" line line))
         (commit-buf (generate-new-buffer " *git-blame-line-commit*")))
    (unwind-protect
        (progn
          ;; 运行 git blame -L line,line file
          (unless (zerop (call-process "git" nil commit-buf nil
                                       "blame" "-L" line-arg buffer-file-name))
            (user-error "git blame 失败，当前目录可能不是 git 仓库"))
          (with-current-buffer commit-buf
            (goto-char (point-min))
            ;; 大致格式：<commit> (<author> <date> <time> ...)
            (when (re-search-forward
                   "^\\([0-9a-f]+\\) (\\([^)]*\\))" nil t)
              (let ((commit (match-string 1))
                    (meta   (match-string 2)))
                (message "Line %d: %s %s" line commit meta))))
      (kill-buffer commit-buf))))
)

(global-set-key (kbd "C-x g c") #'my/git-blame-current-line)


(provide 'init-git)
;;; init-git.el ends here

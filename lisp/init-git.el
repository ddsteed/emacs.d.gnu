;;; init-git.el --- Summary
;;; Commentary:
;;;   git
;;; Code:

(use-package magit
   :ensure t
   :commands magit-status
   :bind
   (
    ("\C-x g" . magit-status)
    ("\C-x m" . magit-show-commit)
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
:after magit)

(use-package magit-imerge
  :ensure t
)
;(global-set-key (kbd "C-x m") 'magit-show-commit)

;(use-package magit-delta
;  :ensure t
;  :hook
;  (magit-mode . magit-delta-mode)
;)

(use-package git-gutter
  :ensure t
  :defer t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :ensure t
  :defer t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))


(provide 'init-git)
;;; init-git.el ends here

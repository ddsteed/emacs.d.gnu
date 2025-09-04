;;; init-git.el --- Summary
;;; Commentary:
;;;   git
;;; Code:

(use-package magit
   :ensure t
   :defer t
   :bind (("\C-x g" . magit-status))
   :config
   (setq magit-auto-revert-mode t)
)

(use-package magit-imerge
  :ensure t
)

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

;;; init-python.el -- Summary
;;; Commentary:
;;;   python language
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq indent-tabs-mode nil)
  (setq python-indent-offset 4)
  (setq flycheck-flake8rc "~/.flake8")
  (setq python-shell-completion-native-disabled-interpreters '("python3"))
  (use-package py-autopep8
    :ensure t)
  :hook
  ((python-mode . py-autopep8-mode)
   (python-mode . jedi:setup)   ; 代码跳转/函数查询
   (python-mode . jedi:ac-setup)
   (python-mode . auto-complete-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python virtual environment
(use-package auto-virtualenv
  :ensure t
  :init
  (use-package pyvenv
    :ensure t)
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv)  ;; If using projectile
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elpy: Emacs Lisp Python Environment
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :hook
  (elpy-mode . flycheck-mode) ;; 添加 flycheck, 替换 flymake
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 代码跳转: 必须设置 jedi:setup，否则要手动执行这个命令
(use-package company-jedi
  :ensure t
  :config
  (add-to-list 'company-backends 'company-jedi)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 取消自动折行
(add-hook 'python-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'python-mode-hook (lambda () (visual-line-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 接受 UTF-8 
(define-coding-system-alias 'UTF-8 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;; conda
; (use-package conda
;   :straight t
;   :config
;   (setq conda-anaconda-home (expand-file-name "/opt/homebrew/Caskroom/miniconda/"))
;   (setq conda-env-home-directory (expand-file-name "/opt/homebrew/Caskroom/miniconda/"))
;   (setq conda-env-subdirectory "envs"))
; 
; (unless (getenv "CONDA_DEFAULT_ENV")
;   (conda-env-activate "base"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-python)

;;; init-python.el ends here

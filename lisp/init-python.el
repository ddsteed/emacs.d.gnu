;;; init-python.el --- Summary
;;; Commentary:
;;;   python
;;; Code:

(use-package python
  :ensure t
  :defer 30
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq indent-tabs-mode nil)
  (setq python-indent-offset 4)
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)
  (setq flycheck-flake8rc "~/.flake8")
  (setq python-shell-completion-native-disabled-interpreters '("python3"))
  (use-package py-autopep8
    :ensure t
    :defer 30
  )
  :hook
  ((python-mode . py-autopep8-mode)
  ;(python-mode . auto-complete-mode)
  ;(python-mode . jedi:setup)             ; 代码跳转/函数查询。 第一次执行的时候，必须手动运行 jedi:setup 命令，系统会在后台执行 jediepcserver
   (python-mode . jedi:ac-setup)
   (python-mode . auto-complete-mode))
  )

(use-package auto-virtualenv
  :ensure t
  :defer 30
  :init
  (use-package pyvenv
    :ensure t
    :defer 30
  )
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv)  ;; If using projectile
  )

(use-package elpy
  :ensure t
  :defer 30
  :init
  (elpy-enable)
  (advice-add 'python-mode :before 'elpy-enable)
  :hook
  (elpy-mode . flycheck-mode) ;; 添加 flycheck, 替换 flymake
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
)

; 关闭 flymake，使用 flycheck
(when (require 'flycheck nil t)
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package company-jedi
  :ensure t
  :defer 30
  :config
  (add-to-list 'company-backends 'company-jedi)
  (add-hook 'python-mode-hook (lambda ()
                                (add-to-list (make-local-variable
                                              'company-backends)
                                             'company-jedi)))
)

;; 取消自动折行
(add-hook 'python-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'python-mode-hook (lambda () (visual-line-mode -1)))

; 接受 UTF-8 
(define-coding-system-alias 'UTF-8 'utf-8)


(provide 'init-python)
;;; init-python.el ends here

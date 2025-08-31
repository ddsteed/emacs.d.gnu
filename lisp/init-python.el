;;; init-python.el --- Summary
;;; Commentary:
;;;   python
;;; Code:

(use-package python
  :ensure t
  :defer t
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
    :defer t
  )
)

(add-to-list 'auto-mode-alist '("\\.\\(env\\|env\\.sample\\)$" . python-mode))

(use-package auto-virtualenv
  :ensure t
  :defer t
  :init
  (use-package pyvenv
    :ensure t
    :defer t
  )
)

(use-package elpy
  :ensure t
  :defer t
  :bind
  (:map elpy-mode-map
        ("C-M-n" . elpy-nav-forward-block)
        ("C-M-p" . elpy-nav-backward-block))
  :init
  (elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
  (setq elpy-shell-echo-output nil)
  (setq elpy-rpc-timeout 10)
  (setq elpy-shell-unbuffered nil)
)

(add-hook 'elpy-mode-hook 'flycheck-mode)
(add-hook 'elpy-mode-hook (lambda () (set (make-local-variable 'company-backends)
                                     '((elpy-company-backend :with company-yasnippet)))))

(use-package company-jedi
  :ensure t
  :defer t
  :config
  (add-to-list 'company-backends 'company-jedi)
)

(setq jedi:complete-on-dot t)                 ; optional

;(add-hook 'python-ts-mode-hook (lambda () (set-fill-column 88)))

;; 取消自动折行
(add-hook 'python-ts-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'python-ts-mode-hook (lambda () (visual-line-mode -1)))

(add-hook 'python-mode-hook    'elpy-mode)
;(add-hook 'python-mode-hook    'eglot-ensure)
(add-hook 'python-mode-hook    'flyspell-prog-mode)
(add-hook 'python-mode-hook    'superword-mode)
(add-hook 'python-mode-hook    'hs-minor-mode)
(add-hook 'python-mode-hook    'jedi:setup)
(add-hook 'python-mode-hook    'flymake-ruff-load)
(add-hook 'python-mode-hook    'ruff-format-on-save-mode)

(add-hook 'python-ts-mode-hook 'elpy-mode)
;(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'flyspell-prog-mode)
(add-hook 'python-ts-mode-hook 'superword-mode)
(add-hook 'python-ts-mode-hook 'hs-minor-mode)
(add-hook 'python-ts-mode-hook 'jedi:setup)
(add-hook 'python-ts-mode-hook 'flymake-ruff-load)
(add-hook 'python-ts-mode-hook 'ruff-format-on-save-mode)

; 接受 UTF-8 
(define-coding-system-alias 'UTF-8 'utf-8)


(provide 'init-python)
;;; init-python.el ends here

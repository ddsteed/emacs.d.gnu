;;; init-python.el -- Summary
;;; Commentary:
;;;   python language
;;; Code:

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq indent-tabs-mode nil)
  (setq python-indent-offset 4)
  (setq flycheck-flake8rc "~/.flake8")
  (use-package py-autopep8
    :ensure t
    )
  :hook
  ((python-mode . jedi:setup))
)

(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" (expand-file-name "/opt/homebrew/Caskroom/miniconda/base/envs"))
  (setq python-shell-interpreter "python3")  ; （可选）更改解释器名字
  (pyvenv-mode t)
  ;; （可选）如果希望启动后激活 miniconda 的 base 环境，就使用如下的 hook
  :hook
  (python-mode . (lambda () (pyvenv-workon "..")))
)

;;
(use-package elpy
  :ensure t
  :defer t
  :init
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")  
  (advice-add 'python-mode :before 'elpy-enable)
  :hook
  (elpy-mode . flycheck-mode) ;; 添加flycheck, 替换flymake
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 显示自动截断
(add-hook 'python-mode-hook (lambda () (setq truncate-lines t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 接受 UTF-8 
(define-coding-system-alias 'UTF-8 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-python)

;;; init-python.el ends here
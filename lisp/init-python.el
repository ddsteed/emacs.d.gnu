;;; init-python.el --- Summary
;;; Commentary:
;;;   python
;;; Code:

(use-package python
  :ensure nil  ;; 内置包
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  ;; Python 缩进设置
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil)

  ;; 禁用内置的 python-flymake，避免 "Cannot find a suitable checker" 错误
  (remove-hook 'flymake-diagnostic-functions 'python-flymake)
)

(add-hook 'python-mode-hook
        (lambda ()
          ;; 完全重置 eldoc 函数列表
          (setq-local eldoc-documentation-functions
                      '(flymake-eldoc-function))))
(use-package envrc
  :defer t
  :hook (after-init . envrc-global-mode))

(use-package direnv
  :defer t
  :config
  (direnv-mode))

(use-package dap-mode
  :defer t
  :config
  (dap-auto-configure-mode) ; 自动配置
  (dap-ui-mode 1)           ; 启用调试 UI
  ;; 安装Python 调试器模板
  (require 'dap-python)
  ;; 设置dap-mode debugpy的路径，通常可以自动找到
  ;; (setq dap-python-debugger 'debugpy)
)
(use-package pyvenv
  :defer t
  :init (setenv "WORKON_HOME" (expand-file-name "~/.emacs.d/elpy/rpc-venv"))
  :config
  ;; 自动激活项目虚拟环境
  (pyvenv-tracking-mode 1)
  (add-hook 'python-mode-hook
            (lambda ()
              (let ((venv-dir (locate-dominating-file default-directory ".venv")))
                (when venv-dir
                  (pyvenv-activate (expand-file-name ".venv" venv-dir)))))))

(use-package elpy
  :defer t
  :init
  (elpy-enable)
  :bind
  (:map elpy-mode-map
        ("C-M-n" . elpy-nav-forward-block)
        ("C-M-p" . elpy-nav-backward-block)
  )
  :hook
  (
    (elpy-mode . flymake-mode)
    (elpy-mode . (lambda ()
                   (set (make-local-variable
                         'company-backends)
                         '((elpy-company-backend :with company-yasnippet))))
    )
  )
  :config
  (pyvenv-mode t)
  ;(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))

  ; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
  (setq elpy-shell-echo-output nil)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-timeout 10)
  (setq elpy-shell-unbuffered nil)
)

(add-hook 'elpy-mode-hook (lambda () (elpy-shell-set-local-shell (elpy-project-root))))
;; Change the virtual environment according to the project
(pyvenv-tracking-mode)
;; 将当前项目根目录加入 PYTHONPATH
(defun my/python-add-project-root-to-path ()
  "Add the project root to PYTHONPATH for Elpy."
  (when (and (derived-mode-p 'python-mode)
             (elpy-module-loaded-p))
    (let ((project-root (elpy-project-root)))
      (when project-root
        (setenv "PYTHONPATH"
                (concat project-root
                        (if (getenv "PYTHONPATH")
                            (concat ":" (getenv "PYTHONPATH"))
                          "")))))))

;; 每次打开 Python 文件时运行
(add-hook 'python-mode-hook 'my/python-add-project-root-to-path)

(add-hook 'elpy-mode-hook 'flymake-mode)
(add-hook 'elpy-mode-hook (lambda () (set (make-local-variable 'company-backends)
                                     '((elpy-company-backend :with company-yasnippet)))))

; (use-package company-jedi
;   :defer t
;   :config
;   (add-to-list 'company-backends 'company-jedi)
; )

; (setq jedi:complete-on-dot t)                 ; optional

(defun elpy-goto-definition-or-rgrep ()
  "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
    (interactive)
    (if (version< emacs-version "25.1")
        (ring-insert find-tag-marker-ring (point-marker))
      (xref-push-marker-stack))
    (condition-case nil (elpy-goto-definition)
        (error (elpy-rgrep-symbol
                   (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))

  ;; xref - 跳转到定义、查找引用
  (use-package xref
    :ensure nil
    :defer t
    :bind (("M-." . xref-find-definitions)
           ("M-?" . xref-find-references)
           ("M-," . xref-pop-marker-stack)))

  ;; 更好的 xref 选择器（结合 consult）
  (setq xref-show-definitions-function #'consult-xref
        xref-show-xrefs-function        #'consult-xref)

  (define-key elpy-mode-map (kbd "C-c .")     'elpy-goto-definition)
  (define-key elpy-mode-map (kbd "C-c d")     'elpy-goto-definition-or-rgrep)
  (define-key elpy-mode-map (kbd "C-x C-.")   'xref-find-definitions)
  (define-key elpy-mode-map (kbd "C-x C-/")   'xref-find-references)
  (define-key elpy-mode-map (kbd "C-x C-,")   'xref-pop-marker-stack)

  ;; imenu - 函数/类列表导航
  (use-package imenu
    :defer t
    :ensure nil
    :bind ("C-c C-i" . imenu))

(use-package hideshow
  :ensure nil
  :defer t
  :hook (python-mode . hs-minor-mode)
  :bind (:map python-mode-map
              ("C-c @ h" . hs-hide-block)
              ("C-c @ s" . hs-show-block)
              ("C-c @ t" . hs-toggle-hiding)))

(add-hook 'python-ts-mode-hook (lambda () (set-fill-column 88)))

;; 取消自动折行
(add-hook 'python-ts-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'python-ts-mode-hook (lambda () (visual-line-mode -1)))

(add-hook 'python-mode-hook (lambda () (set-fill-column 88)))
(add-hook 'python-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'python-mode-hook (lambda () (visual-line-mode -1)))

(add-hook 'python-mode-hook    'elpy-mode)
(add-hook 'python-mode-hook    'eglot-ensure)
(add-hook 'python-mode-hook    'flyspell-prog-mode)
(add-hook 'python-mode-hook    'superword-mode)
(add-hook 'python-mode-hook    'hs-minor-mode)
(add-hook 'python-mode-hook    'ruff-format-on-save-mode)

(add-hook 'python-ts-mode-hook 'elpy-mode)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'flyspell-prog-mode)
(add-hook 'python-ts-mode-hook 'superword-mode)
(add-hook 'python-ts-mode-hook 'hs-minor-mode)
(add-hook 'python-ts-mode-hook 'ruff-format-on-save-mode)

; (define-key python-mode-map (kbd "M-.") 'xref-find-definitions)
; (define-key python-mode-map (kbd "M-?") 'xref-find-references)
; (define-key python-mode-map (kbd "M-,") 'xref-pop-marker-stack)

; 接受 UTF-8 
(define-coding-system-alias 'UTF-8 'utf-8)


(provide 'init-python)
;;; init-python.el ends here

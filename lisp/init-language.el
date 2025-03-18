;;; init-language.el --- Summary
;;; Commentary:
;;;  通用编程语言设置
;;; Code:

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

; 关闭 flymake，使用 flycheck
(when (require 'flycheck nil t)
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ggtags :ensure t)

(add-hook 'c-mode-common-hood
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'python-mode)
              (ggtags-mode 1))))

;; mwim: mwim stands for Move Where I Mean.
(use-package mwim :ensure t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode))

(defun my-highlighter (level responsive display)
  (if (> 2 level)
      nil
    (highlight-indent-guides--highlighter-default level responsive display)))
(setq highlight-indent-guides-highlighter-function 'my-highlighter)

(use-package indent-guide
  :ensure t
  :hook (prog-mode . indent-guide-mode))

(use-package aggressive-indent
  :ensure t)

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(add-hook 'org-mode-hook #'aggressive-indent-mode)

(global-aggressive-indent-mode 1)

(add-to-list 'aggressive-indent-excluded-modes 'html-mode)
(add-to-list 'aggressive-indent-excluded-modes 'org-mode)

(add-to-list
 'aggressive-indent-dont-indent-if
 '(and (derived-mode-p 'c++-mode)
       (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                           (thing-at-point 'line)))))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-tag-follow-mode)
  (treemacs-project-follow-mode)
  :bind
  (:map treemacs-mode-map
    ("/" . treemacs-advanced-helpful-hydra)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

;; treemacs: 工作区管理
(global-set-key (kbd "M-0") 'treemacs-select-window)
(global-set-key (kbd "C-x t 1") 'treemacs-delete-other-windows)
(global-set-key (kbd "C-x t t") 'treemacs)
(global-set-key (kbd "C-x t q") 'treemacs-quit)
(global-set-key (kbd "C-x t B") 'treemacs-bookmark)
(global-set-key (kbd "C-x t C-t") 'treemacs-find-file)
(global-set-key (kbd "C-x t M-t") 'treemacs-find-tag)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (css-mode . css-ts-mode)
        (cpp-mode . cpp-ts-mode)
        (js2-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (markdown-mode . markdown-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (python-mode . python-ts-mode)))

(provide 'init-language)

;;; init-language.el ends here

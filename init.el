;;; init.el --- Summary
;;; Commentary:
;;;    Emacs init setup
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 把目录 lisp/添加到搜索路径中去
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 把自定义的插件目录添加进来
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/addons")
; (add-to-list 'load-path (expand-file-name "~/.emacs.d/addons"))

;; 下面每一个被 require 的 feature 都对应一个 lisp/目录下的同名 elisp 文件，
;; 例如 init-global.el、init-elpa.el

(require 'init-elpa)     ;; package repository
(require 'init-execpath) ;; eshell path
(require 'init-look)     ;; look face
(require 'init-global)   ;; global variables
(require 'init-evil)     ;; evil mode
(require 'init-language) ;; programming language style
(require 'init-org)      ;; org mode
(require 'init-roam)     ;; org-roam mode
(require 'init-markdown) ;; markdown mode
(require 'init-tex)      ;; TeX/LaTeX mode
(require 'init-cal)      ;; calendar
(require 'init-git)      ;; magit
(require 'init-mterm)    ;; multi-term
;(require 'init-lsp)      ;; LSP
;(require 'init-lspb)     ;; lsp-bridge
;(require 'init-eaf)      ;; emacs-application-framework

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 设置一个空的 custom file，这样系统默认的 custom 就不会每次自动写入 init.el
(setq custom-file "~/.emacs.d/custom.el")
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server start automatically
(use-package server)
(unless (server-running-p) (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init)

;;; init.el ends here

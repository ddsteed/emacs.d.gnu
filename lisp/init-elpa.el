;;; init-elpa.el --- Summary
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package) ;; package必须首先手动安装，以后碰到use-package时再安装

;;;;;;;;;
;; Add useful package repo
(add-to-list 'package-archives '("gnu"          . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/") t)
; melpa-stable 中的某些包缺失了功能，尽量不用。
; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;;;;;;;;;
;; 启动的时候先不加载各种package，等真需要的时候再加
(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;
;; Run package-refresh-contents if first start.
;; use folder "var" to check if it is the first start, see no-littering.

; (unless (file-exists-p (expand-file-name "var" user-emacs-directory))
;  (package-refresh-contents))

;;;;;;;;;
;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-elpa)

;;; init-elpa.el ends here

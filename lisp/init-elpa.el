;;; init-elpa.el --- Summary
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package) ;; package 必须首先手动安装，以后碰到 use-package 时再安装

;;;;;;;;;
;; Add useful package repo
(add-to-list 'package-archives '("gnu"          . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/") t)

; melpa-stable 中的某些包缺失了功能，尽量不用。
; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;;;;;;;;;
;; 启动的时候先不加载各种 package，等真需要的时候再加
(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;
;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-elpa)

;;; init-elpa.el ends here

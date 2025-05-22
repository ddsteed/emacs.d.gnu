;;; init-elpa.el --- Summary
;;; Commentary:
;;;   package management
;;; Code:

(require 'package) 

(add-to-list 'package-archives '("gnu"    . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa"  . "http://melpa.org/packages/") t)

;; 启动的时候先不加载各种 package，等真需要的时候再加
(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;
;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
  

(provide 'init-elpa)
;;; init-elpa.el ends here

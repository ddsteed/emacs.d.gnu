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
(use-package auto-package-update
 :ensure t
 :config
 (setq auto-package-update-delete-old-versions t
       auto-package-update-interval 4)
 (auto-package-update-maybe))

(use-package paradox
  :ensure t
  :defer t
  :init
  (setq paradox-github-token t)
  (setq paradox-execute-asynchronously t)
  (setq paradox-automatically-star t))


(provide 'init-elpa)
;;; init-elpa.el ends here

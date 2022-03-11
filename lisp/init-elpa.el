;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

;; 启动的时候先不加载各种package，等真需要的时候再加
(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add useful package repo
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")              ;; GNU ELPA repository (Offical)
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")          ;; NonGNU ELPA 
        ("melpa" . "https://melpa.org/packages/")               ;; MELPA repository
        ("org" . "http://orgmode.org/elpa/")))                  ;; Org-mode's repository

;; Install use-package if not available
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq warning-minimum-level :emergency)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-elpa)

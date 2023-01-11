;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package) ;; package必须首先手动安装，以后碰到use-package时再安装

;; 启动的时候先不加载各种package，等真需要的时候再加
(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add useful package repo
(setq package-archives
      '(
       ;("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
       ;("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
       ;("org-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ("melpa"  . "https://melpa.org/packages/")              ;; MELPA repository
        ("gnu"    . "https://elpa.gnu.org/packages/")           ;; GNU ELPA repository (Offical)
       ;("org"    . "http://orgmode.org/elpa/")                 ;; Org-mode's repository
))

;; Run package-refresh-contents if first start.
;; use folder "var" to check if it is the first start, see no-littering.
(unless (file-exists-p (expand-file-name "var" user-emacs-directory))
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
;; this i only needed once
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(setq use-package-verbose t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-elpa)

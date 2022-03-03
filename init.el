;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 把目录lisp/添加到搜索路径中去
(add-to-list
    'load-path 
    (expand-file-name "lisp" user-emacs-directory))

;; 下面每一个被require的feature都对应一个lisp/目录下的同名elisp文件，
;; 例如init-global.el、init-elpa.el

(require 'init-elpa)     ;; package installation
(require 'init-global)   ;; global variables
(require 'init-execpath) ;; eshell path
(require 'init-look)     ;; look face
(require 'init-language) ;; programming language style
(require 'init-org)      ;; org mode
(require 'init-markdown) ;; markdown mode
(require 'init-cal)      ;; calendar
(require 'init-git)      ;; magit 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 设置一个空的custom file，这样系统默认的custom 就不会每次自动写入inti.el
(setq custom-file "~/.emacs.d/custom.el")
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server start automatically
(require 'server)
(unless (server-running-p) (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init)

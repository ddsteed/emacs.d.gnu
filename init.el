;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 把目录lisp/添加到搜索路径中去
(add-to-list
    'load-path 
    (expand-file-name "lisp" user-emacs-directory))

;; 下面每一个被require的feature都对应一个lisp/目录下的同名elisp文件，
;; 例如init-global.el、init-elpa.el

(require 'init-elpa)     ;; package repository
(require 'init-execpath) ;; eshell path
(require 'init-global)   ;; global variables
(require 'init-look)     ;; look face
(require 'init-language) ;; programming language style
(require 'init-org)      ;; org mode
(require 'init-markdown) ;; markdown mode
(require 'init-cal)      ;; calendar
(require 'init-git)      ;; magit
(require 'init-project)  ;; project management
(require 'init-evil)     ;; evil mode
(require 'init-mterm)    ;; multi-term

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 设置一个空的custom file，这样系统默认的custom 就不会每次自动写入inti.el
(setq custom-file "~/.emacs.d/custom.el")
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server start automatically
(require 'server)
(unless (server-running-p) (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/Users/fengh/Work/GTD/Inbox.org" "/Users/fengh/Work/GTD/Inbox.org_archive" "/Users/fengh/Work/GTD/Personal.org" "/Users/fengh/Work/GTD/Personal.org_archive" "/Users/fengh/Work/GTD/Read.org" "/Users/fengh/Work/GTD/Read.org_archive" "/Users/fengh/Work/GTD/Research.org" "/Users/fengh/Work/GTD/Research.org_archive" "/Users/fengh/Work/GTD/Startup.org" "/Users/fengh/Work/GTD/Startup.org_archive" "/Users/fengh/Work/GTD/Study.org" "/Users/fengh/Work/GTD/Study.org_archive" "/Users/fengh/Work/GTD/Tickler.org" "/Users/fengh/Work/GTD/Work.org" "/Users/fengh/Work/GTD/Work.org_archive")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((nil (:height 100)))))

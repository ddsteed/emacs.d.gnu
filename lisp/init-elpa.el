;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 启动的时候先不加载各种package，等真需要的时候再加
(setq package-enable-at-startup nil)
(package-initialize)

;; 防止反复调用 package-refresh-contents 会影响加载速度
(when (not package-archive-contents)
  (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-x list-packages    (list packages)
(require 'package)

;; (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
;;                          ("melpa" . "http://elpa.emacs-china.org/melpa/")
;;                         ))

(setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")
                         ("melpa" . "http://elpa.zilongshanren.com/melpa/")
                         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-elpa)

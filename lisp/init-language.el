;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(require 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C和C++语言编程风格
(add-hook 'c-mode-hook
	  '(lambda ()
 	     (c-set-style "Stroustrup")
;;	     (c-set-style "BSD")
;; 	     (c-set-style "GNU")
;; 	     (c-toggle-auto-state) ; 加上自动开始新行的功能
))   

(add-hook 'c++-mode-hook
	  '(lambda ()
	     (c-set-style "Stroustrup")
;;	     (c-set-style "BSD")
;;	     (c-toggle-auto-state) ; 加上自动开始新行的功能
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FORTRAN
(setq fortran-comment-indent-char "")
(setq fortran-comment-region "C *")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto load cmake-mode for CMakeLists.txt
;; (require 'cmake-mode)
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wolfram mathematica
(require 'init-wolfram)  ;; wolfram mathematica

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell script

; 不显示过长的行
(add-hook 'sh-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'shell-script-mode-hook (lambda () (setq truncate-lines t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-language)

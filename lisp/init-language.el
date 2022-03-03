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
;; Python

;; Configuration of Python IDE  
;; https://github.com/jorgenschaefer/elpy  
(require 'elpy nil t)  
(elpy-enable)  

(setq elpy-rpc-python-command "/opt/Homebrew/bin/python3")  ;; python3

;; for ipython notebook
(defvar myPackages
  '(better-defaults
    ein ;; add the ein package (Emacs ipython notebook)
    py-autopep8))

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

(provide 'init-language)

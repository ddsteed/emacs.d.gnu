;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wolfram mode

;; (add-to-list 'load-path "~/.emacs.d/lisp/wolfram-mode")

;; (autoload 'wolfram-mode "wolfram-mode" nil t)
;; (autoload 'run-wolfram "wolfram-mode" nil t)
;; (setq wolfram-program "/Applications/Mathematica.app/Contents/MacOS/MathKernel")
;; (add-to-list 'auto-mode-alist '("\.m$" . wolfram-mode))
;; (setq wolfram-path "~/.mathematica/Applications")

;; (require 'wolfram-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "~/.emacs.d/lisp/math")
;; (require 'math)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "~/.emacs.d/lisp/xah-wolfram-mode")
;; (require 'xah-wolfram-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'wolfram)

(setq wolfram-alpha-app-id "4EEGR9-2J4263E6QU")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-wolfram)

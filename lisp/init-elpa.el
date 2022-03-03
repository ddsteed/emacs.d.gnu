;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beginning with version 24.1 Emacs includes a package management
;; facility known as Elpa or package.el. Using an Elpa package
;; repository is the easiest and recommended way to install and update
;; Magit and its dependencies. Among other things using package.el is
;; recommended because that automatically takes care of installing
;; dependencies.

;; M-x list-packages    (list packages)
(require 'package)

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
;                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(provide 'init-elpa)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

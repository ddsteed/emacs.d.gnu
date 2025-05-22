;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(let ((minver "28.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "29.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path "~/.emacs.d.rds/elpa")
(add-to-list 'load-path "~/.emacs.d.rds/addons")
;; Adjust garbage collection threshold for early startup
(setq gc-cons-threshold (* 128 1024 1024))
(setq custom-file (locate-user-emacs-file "custom.el"))
(load "server")
(unless (server-running-p) (server-start))

(require 'init-elpa)
(require 'init-exec-path)
(require 'init-frame-hooks)
(require 'init-look)
(require 'init-general)
(require 'init-buffer)
(require 'init-chinese)
(require 'init-vi)
(require 'init-terminal)
(require 'init-calendar)
(require 'init-program)
(require 'init-eglot)
(require 'init-company)
(require 'init-cpp)
(require 'init-cmake)
(require 'init-fortran)
(require 'init-git)
(require 'init-lisp)
(require 'init-markdown)
(require 'init-python)
(require 'init-ruff)
(require 'init-wolfram)
(require 'init-tex)
(require 'init-org)
(require 'init-roam)
(require 'init-ai)

(provide 'init)
;;; init.el ends here

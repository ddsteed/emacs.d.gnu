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

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
        (lambda ()
          (message "Emacs ready in %s with %d garbage collections."
                   (format "%.2f seconds"
                           (float-time
                            (time-subtract after-init-time before-init-time)))
                   gcs-done)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path "~/.emacs.d.rds/elpa")
(add-to-list 'load-path "~/.emacs.d.rds/addons")

(setq custom-file (locate-user-emacs-file "custom.el"))

(use-package server
  :ensure t
  :defer 5
  :config
  (autoload 'server-running-p "server")
  (unless (server-running-p) (server-start))
)

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
(require 'init-lsp)
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
(require 'init-ref)
(require 'init-ai)

;(defvar bootstrap-version)
;(let ((bootstrap-file
;       (expand-file-name
;        "straight/repos/straight.el/bootstrap.el"
;        (or (bound-and-true-p straight-base-dir)
;            user-emacs-directory)))
;      (bootstrap-version 7))
;  (unless (file-exists-p bootstrap-file)
;    (with-current-buffer
;        (url-retrieve-synchronously
;         "https://radian-software.github.io/straight.el/install.el"
;         'silent 'inhibit-cookies)
;      (goto-char (point-max))
;      (eval-print-last-sexp)))
;  (load bootstrap-file nil 'nomessage))

;; 启动时全屏
;; Default frame settings. This is actually maximized, not full screen.
(push '(fullscreen . maximized) initial-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)

(provide 'init)
;;; init.el ends here

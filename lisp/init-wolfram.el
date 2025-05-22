;;; init-wolfram.el --- Summary
;;; Commentary:
;;;   wolfram mathematica
;;; Code:
(use-package wolfram)

(setq wolfram-alpha-app-id "4EEGR9-2J4263E6QU")
(setq wolfram-use-dark-version t)
(setq wolfram-alpha-magnification-factor 1.5)

(add-to-list 'load-path "~/.emacs.d.rds/addons/xah-wolfram-mode")
(require 'xah-wolfram-mode)

(autoload 'wolfram-mode "wolfram-mode" nil t)
(autoload 'run-wolfram "wolfram-mode" nil t)
(setq wolfram-program "/Applications/Wolfram\ Engine.app/Contents/MacOS/WolframKernel")

(add-to-list 'auto-mode-alist '("\.m$"  . wolfram-mode))
(add-to-list 'auto-mode-alist '("\.wl$" . wolfram-mode))
(setq wolfram-path "~/.wolfram/Package")


(provide 'init-wolfram)
;;; init-wolfram.el ends here

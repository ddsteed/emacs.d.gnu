;;; init-wolfram.el --- Summary
;;; Commentary:
;;;   wolfram mathematica
;;; Code:

(use-package wolfram)

(setq wolfram-alpha-app-id "4EEGR9-2J4263E6QU")

(add-to-list 'load-path "~/.emacs.d/addons/xah-wolfram-mode")
(require 'xah-wolfram-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-wolfram)

;;; init-wolfram.el ends here

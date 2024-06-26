;;; init-exepath.el --- Summary
;;; Commentary:
;;;   eshell path
;;; Code:

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :init
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(require 'cache-path-from-shell)

(setenv "feng_bin" "/Users/fenghao/Work/home1/feng/BIN")
(setenv "local_bin" "/usr/local/bin")
(setenv "PATH" (concat (getenv "PATH")
                       ":" (getenv "feng_bin")
                       ":" (getenv "local_bin")
                       ))

;; call $PATH from working shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(when (daemonp)
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-execpath)

;;; init-execpath.el ends here

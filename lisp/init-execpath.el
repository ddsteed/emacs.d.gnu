;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell path

(setenv "andconda_path" "/opt/anaconda3")
(setenv "feng_bin" "/Users/fenghao/Work/home1/feng/BIN")
(setenv "local_bin" "/usr/local/bin")
(setenv "PATH" (concat (getenv "PATH")
                       ":" (getenv "anaconda_path") "/bin" 
                       ":" (getenv "feng_bin")
                       ":" (getenv "local_bin")
                       ))

;; call $PATH from working shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-execpath)

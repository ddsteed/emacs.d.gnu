;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-publish
;;;;;;;;
; org mode to html config
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-htmlize-output-type 'inline-css ;; 保留代码块高亮
;     org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://files.cnblogs.com/csophys/orgstyle.css\"/>"
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/Users/fengh/.org/config/orgstyle.css\"/>"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-publish)

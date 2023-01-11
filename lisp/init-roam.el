;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-Roam basic configuration
(setq org-directory (concat (getenv "HOME") "/Documents/RDS/NOTES/Org"))
(setq org-roam-directory (concat (getenv "HOME") "/Documents/RDS/NOTES/Org/Roam"))

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-setup)
  :bind (("C-c r f" . org-roam-node-find)
         ("C-c r r" . org-roam-node-random)		    
         (:map org-mode-map
               (("C-c r i" . org-roam-node-insert)
                ("C-c r o" . org-id-get-create)
                ("C-c r t" . org-roam-tag-add)
                ("C-c r a" . org-roam-alias-add)
                ("C-c r l" . org-roam-buffer-toggle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-roam)

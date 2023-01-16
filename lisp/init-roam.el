;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-Roam basic configuration
(setq org-directory (concat (getenv "HOME") "/Documents/RDS/NOTES/Org"))

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename "~/Documents/RDS/NOTES/Org/Roam"))
  (find-file-visit-truename t)
  (add-hood 'after-init-hook 'org-roam-mode)
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

;;;;;;;
;; org roam keybindings
(global-set-key (kbd "C-c C-r") 'org-roam-capture)

;;;;;;;
;; to ensure that org-roam is available on startup
(org-roam-db-autosync-mode)

;;;;;;;
;; My Org-roam capture templates. There is one for each zettel type.
(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n#+filetags: :draft:\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org" "#+title: ${title}\n#+filetags: :reference:\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)))


; add org capture file
(add-to-list 'org-capture-templates 
             '("s" "Slipbox" entry (file "~/Documents/RDS/NOTES/Org/Roam/Inbox.org")
               "* %?\n"))

(defun rds/org-capture-slipbox ()
  (interactive)
  (org-capture nil "s"))

;;;;;;;
;; Creating the property “type” on my nodes.
(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

;;;;;;;
;; Modifying the display template to show the node “type”
(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

;;;;;;;
;; roam sqlite database
(use-package sqlite3)
(setq org-roam-database-connector 'sqlite3)

;;;;;;;
;;;;;;;
;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-roam)

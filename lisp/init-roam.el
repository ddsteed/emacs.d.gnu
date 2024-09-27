;;; init-roam.el --- Summary
;;; Commentary:
;;;   org-roam setup
;;; Code:

;; Org-Roam basic configuration
(setq org-directory (concat (getenv "HOME") "/Documents/RDS/NOTES/Org"))

(use-package org-roam
  :ensure t
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename "~/Documents/RDS/NOTES/Org/Roam"))
  (find-file-visit-truename t)
  (add-hood 'after-init-hook 'org-roam-mode)
  (org-roam-database-connector 'sqlite-builtin)
  :config
  (org-roam-setup)
  (org-roam-db-autosync-enable)
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
; (org-roam-db-autosync-mode)

;;;;;;;
;; My Org-roam capture templates. There is one for each zettel type.
(setq org-roam-capture-templates
      '(
        ("f" "fleeting" plain
         "%?"
         :if-new (file+head "fleeting/${slug}.org"
                            "#+title: ${title}\n#+filetags: :fleeting:\n")
         :immediate-finish t
         :unnarrowed t)
         ("p" "progress" plain "%?"
         :if-new
         (file+head "progress/${title}.org" "#+title: ${title}\n#+filetags: :progress:\n")
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
         :unnarrowed t)
        )
)


; add org capture file
(add-to-list 'org-capture-templates 
             '("s" "Slipbox" entry (file "~/Documents/RDS/NOTES/Org/Roam/Inbox.org")
               "* %?\n"))

(defun rds/org-capture-slipbox ()
  (interactive)
  (org-capture nil "s"))

;;;;;;;
;; Creating the property “type” on my nodes.
(with-eval-after-load 'org-roam
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error ""))))

;;;;;;;
;; Modifying the display template to show the node “type”
(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} "
              (propertize "${tags:10}" 'face 'org-tag)))

;;;;;;;
;; To configure what sections are displayed in the buffer
(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            ;; #'org-roam-unlinked-references-section
            ))

;;;;;;;
;;  control how the pop-up buffer is displayed
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The same completions can be triggered anywhere for the symbol at point if not within a bracketed link
(setq org-roam-completion-everywhere t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (use-package org-roam-protocol
;  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use journal
(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/Documents/RDS/NOTES/Org/Journal") 
  (org-journal-date-format "%A, %d %B %Y"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; screenshot and yank images from the web into your notes
(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uses Mathpix’s API to convert clips into latex equations
(use-package mathpix.el
  :load-path "~/.emacs.d/addons"
  :custom ((mathpix-app-id "app-id")
           (mathpix-app-key "app-key"))
  :bind
  ("C-x m" . mathpix-screenshot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; export
; (use-package org-roam-export
;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; garbage collection
(setq org-roam-db-gc-threshold most-positive-fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a nice interface for browsing and filtering org-roam notes
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-roam)

;;; init-roam.el ends here

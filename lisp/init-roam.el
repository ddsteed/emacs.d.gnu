;;; init-roam.el --- Summary
;;; Commentary:
;;;   Org Roam
;;; Code:

(setq org-directory (concat (getenv "HOME") "/Documents/RDS/NOTES/Org"))

 (use-package org-roam
   :ensure t
   :after org
   :init
   (setq org-roam-v2-ack t) 
   (org-roam-db-sync) 
   :custom
   (org-roam-directory (file-truename "~/Documents/RDS/NOTES/Org/Roam"))
   (find-file-visit-truename t)
   (add-hood 'after-init-hook 'org-roam-mode)
   (org-roam-database-connector 'sqlite-builtin)
   (org-roam-db-location "~/.emacs.d.rds/org-roam.db")
   :config
   (org-roam-setup)
   (org-roam-db-autosync-enable)
)
(global-set-key (kbd "C-c C-r") 'org-roam-capture)

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

(add-to-list 'org-capture-templates 
             '("s" "Slipbox" entry (file "~/Documents/RDS/NOTES/Org/Roam/Inbox.org")
               "* %?\n"))

(defun rds/org-capture-slipbox ()
  (interactive)
  (org-capture nil "s"))

(with-eval-after-load 'org-roam
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error ""))))

(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} "
              (propertize "${tags:10}" 'face 'org-tag)))

;; To configure what sections are displayed in the buffer
(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            ;; #'org-roam-unlinked-references-section
            ))

;;  control how the pop-up buffer is displayed
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(setq org-roam-completion-everywhere t)

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/Documents/RDS/NOTES/Org/Journal") 
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(use-package mathpix.el
  :load-path "~/.emacs.d.rds/addons"
  :custom ((mathpix-app-id "app-id")
           (mathpix-app-key "app-key"))
  :bind
  ("C-x m" . mathpix-screenshot))

(setq org-roam-db-gc-threshold most-positive-fixnum)

(add-hook 'org-mode-hook
           (lambda ()
             (local-set-key (kbd "C-c r f") 'org-roam-node-find)
             (local-set-key (kbd "C-c r r") 'org-roam-node-random)
             (local-set-key (kbd "C-c r i") 'org-roam-node-insert)
             (local-set-key (kbd "C-c r o") 'org-id-get-create)
             (local-set-key (kbd "C-c r t") 'org-roam-tag-add)
             (local-set-key (kbd "C-c r a") 'org-roam-alias-add)
             (local-set-key (kbd "C-c r l") 'org-roam-buffer-toggle)
           )
)

(provide 'init-roam)

;;; init-roam.el ends here

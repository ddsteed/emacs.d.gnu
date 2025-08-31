;;; init-roam.el --- Summary
;;; Commentary:
;;;   Org Roam
;;; Code:

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
   (org-roam-completion-everywhere t)
   :config
   (org-roam-setup)
   (org-roam-db-autosync-enable)
)

;; 融合 org
;; 任意 mode 都可以找到笔记
(global-set-key (kbd "C-c r f") 'org-roam-node-find)

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

(setq org-roam-capture-templates
     '(("f" "fleeting" plain "%?" :if-new (file+head "fleeting/${slug}.org" "#+title: ${title}\n#+filetags: :fleeting:\n")
         :immediate-finish t :unnarrowed t)
         ("p" "progress" plain "%?" :if-new (file+head "progress/${title}.org" "#+title: ${title}\n#+filetags: :progress:\n")
         :immediate-finish t :unnarrowed t)
        ("r" "reference" plain "%?" :if-new (file+head "reference/${title}.org" "#+title: ${title}\n#+filetags: :reference:\n")
         :immediate-finish t :unnarrowed t)
        ("a" "article" plain "%?" :if-new (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t :unnarrowed t)
     )
)

(add-to-list 'org-roam-capture-templates
   '("i" "Ideas" plain "%?" :if-new (file+head "Zettel/Slip/Ideas/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :ideas:\n\n")
    "* %i%? \n %U" :immediate-finish t :unnarrowed t))

(add-to-list 'org-roam-capture-templates
   '("e" "Exerpts" plain "%?" :if-new (file+head "Zettel/Slip/Exerpts/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :exerpts:\n")
    "* %i%? \n %U" :immediate-finish t :unnarrowed t))

(add-to-list 'org-roam-capture-templates
   '("l" "Literature" plain "%?" :if-new (file+head "Zettel/Literature/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :literal:\n")
    "* %i%? \n %U" :immediate-finish t :unnarrowed t))

(add-to-list 'org-roam-capture-templates
   '("P" "Permanent" plain "%?" :if-new (file+head "Zettel/Permanent/${slug}.org" "#+title: ${title}\n#+filetags: :permanent:\n")
    "* %i%? \n %U" :immediate-finish t :unnarrowed t))

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
  :defer t
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(setq org-roam-completion-everywhere t)

(use-package corfu
  :ensure t
  :defer t
  :init
  (progn
    (setq corfu-auto t)
    (setq corfu-cycle t)
    (setq corfu-quit-at-boundary t)
    (setq corfu-quit-no-match t)
    (setq corfu-preview-current nil)
    (setq corfu-min-width 80)
    (setq corfu-max-width 100)
    (setq corfu-auto-delay 0.2)
    (setq corfu-auto-prefix 1)
    (setq corfu-on-exact-match nil)
    (global-corfu-mode)
  )
)

(use-package org-journal
  :ensure t
  :defer t
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/Documents/RDS/NOTES/Org/Journal") 
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-download
  :after org
  :defer t
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(use-package mathpix.el
  :defer t
  :load-path "~/.emacs.d.rds/addons"
  :custom ((mathpix-app-id "app-id")
           (mathpix-app-key "app-key"))
  :bind
  ("C-x m" . mathpix-screenshot))

(setq org-roam-db-gc-threshold most-positive-fixnum)

(use-package org-roam-ui
  :after org-roam
  :defer t
  :commands org-roam-ui-mode
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save nil
        org-roam-ui-open-on-start t)
  (add-to-list 'desktop-minor-mode-table
               '(org-roam-ui-mode nil))
  (add-to-list 'desktop-minor-mode-table
               '(org-roam-ui-follow-mode nil))
)


(provide 'init-roam)
;;; init-roam.el ends here

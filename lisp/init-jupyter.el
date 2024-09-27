;;; init-jupyter.el --- Summary
;;; Commentary:
;;;   jupyter setup
;;; Code:


(use-package conda
  :defer t
  :init
  (setq conda-anaconda-home (expand-file-name "/opt/homebrew/Caskroom/miniforge/")
	    conda-env-home-directory (expand-file-name "/opt/homebrew/Caskroom/miniforge/"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

(use-package jupyter
  :defer t
  :init
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "py"))))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (jupyter . t)))

(add-to-list 'org-src-lang-modes '("jupyter" . python))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (jupyter . t)
   ;; other languages..
   ))

(org-babel-jupyter-override-src-block "python")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ein
(use-package ein
  :ensure nil
  :init
  (add-hook 'ein:notebook-mode-hook 'jedi:setup)
  :config
  (setq ein:output-area-inlined-images t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs"
  (interactive)
  (jupyter-available-kernelspecs t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ipynb-to-markdown
(defun ipynb-to-markdown (file)
  (interactive "f")
  (let* ((data (with-temp-buffer
                 (insert-file-literally file)
                 (json-parse-string (buffer-string)
                                    :object-type 'alist
                                    :array-type 'list)))
         (metadata (alist-get 'metadata data))
         (kernelspec (alist-get 'kernelspec metadata))
         (language (alist-get 'language kernelspec)))
    (pop-to-buffer "ipynb-as-markdown")
    ;; (when (featurep 'markdown-mode)
    ;;   (markdown-mode))
    (dolist (c (alist-get 'cells data))
      (let* ((contents (alist-get 'source c))
             (outputs (alist-get 'outputs c)))
        (pcase (alist-get 'cell_type c)
          ("markdown"
           (when contents
             (mapcar #'insert contents)
             (insert "\n\n")))
          ("code"
           (when contents
             (insert "```")
             (insert language)
             (insert "\n")
             (mapcar #'insert contents)
             (insert "\n```\n\n")
             (dolist (x outputs)
               (when-let (text (alist-get 'text x))
                 (insert "```stdout\n")
                 (insert (mapconcat #'identity text ""))
                 (insert "\n```\n\n"))
               (when-let (data (alist-get 'data x))
                 (when-let (im64 (alist-get 'image/png data))
                   (let ((imdata (base64-decode-string im64)))
                     (insert-image (create-image imdata 'png t)))))
               (insert "\n\n")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ipynb to html
(defun rds/shr-open-ipynb (filename)
  "Open ipynb file as html."
  (interactive)
  (let* ((shortname (file-name-nondirectory filename))
         (command "jupyter nbconvert --to html --log-level WARN --stdout --stdin"))
    (with-temp-buffer
      (insert-file-contents filename)
      (shell-command-on-region (point-min) (point-max) command nil 'no-mark)
      (shr-render-buffer (current-buffer))
      )
    (with-current-buffer "*html*"
      (rename-buffer shortname 'unique)
      (read-only-mode t))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-jupyter)

;;; init-jupyter.el ends here

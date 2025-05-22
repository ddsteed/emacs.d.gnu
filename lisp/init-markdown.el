;;; init-markdown.el --- Summary
;;; Commentary:
;;;   markdown
;;; Code:

(use-package markdown-mode
 :ensure t
 :defer 30
 :commands (markdown-mode gfm-mode)
 :mode (("README\\.md\\'" . gfm-mode)
        ("\\.md\\'" . markdown-mode)
        ("\\.markdown\\'" . markdown-mode))
 :init (setq markdown-command "/usr/local/bin/pandoc"))

(use-package markdown-ts-mode
 :ensure t
 :defer 30
 :mode ("\\.md\\'" . markdown-ts-mode)
 :defer 't
)


(provide 'init-markdown)
;;; init-markdown.el ends here

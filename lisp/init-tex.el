;;; init-tex.el --- Summary
;;; Commentary:
;;;   tex
;;; Code:

(use-package tex
 :ensure auctex
 :defer t
 )

(use-package auctex
  :defer t
  :ensure t
)

(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)       ;;
(add-hook 'LaTeX-mode-hook 'turn-on-auctex)        ;;
(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode) ;;
                                                   ;;
(setq org-latex-compiler "xelatex")                ;;


(provide 'init-tex)
;;; init-tex.el ends here

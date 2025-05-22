;;; init-tex.el --- Summary
;;; Commentary:
;;;   tex
;;; Code:

(add-hook 'LaTeX-mode-hood 'turn-on-cdlatex)       ;;
(add-hook 'LaTeX-mode-hood 'turn-on-auctex)        ;;
(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode) ;;
                                                   ;;
(setq org-latex-compiler "xelatex")                ;;


(provide 'init-tex)
;;; init-tex.el ends here

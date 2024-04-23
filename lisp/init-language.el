;;; init-language.el --- Summary
;;; Commentary:
;;;    for programming language
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 代码检查工具
(use-package flycheck
  :ensure t
  :hook                        ; 为模式设置 hook
  (prog-mode . flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rainbow: 不同颜色标记多级括号
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gtags
(use-package ggtags)

(global-set-key (kbd "M-.") 'gtags-find-tag)
(global-set-key (kbd "M-,") 'gtags-find-rtag)
(global-set-key (kbd "M-g M-f") 'gtags-find-file)
(global-set-key (kbd "M-g M-s") 'gtags-find-symbol)
(global-set-key (kbd "M-g M-u") 'gtags-update)

(add-hook 'c-mode-common-hood
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'python-mode)
              (ggtags-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
(require 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
(add-hook 'c-mode-hook
	      (lambda ()
	        (setq truncate-lines t)
 	        (c-set-style "Stroustrup")
 	        (c-toggle-auto-newline) ; 加上自动开始新行的功能
))

(add-hook 'c++-mode-hook
	      (lambda ()
	        (setq truncate-lines t)
	        (c-set-style "Stroustrup")
            (c-toggle-auto-newline) ; 加上自动开始新行的功能
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FORTRAN
(setq fortran-comment-indent-char "")
(setq fortran-comment-region "C *")

(add-hook 'fortran-mode-hook
	      (lambda ()
	        (setq truncate-lines t)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wolfram mathematica
(require 'init-wolfram)  ;; wolfram mathematica

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp

; 自动折行
(add-hook 'lisp-mode-hook (lambda () (setq truncate-lines t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell script

; 自动折行
(add-hook 'sh-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'shell-script-mode-hook (lambda () (setq truncate-lines t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown
(require 'init-markdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jupyter
(require 'init-jupyter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plantuml 画图

;; plantuml is based java, so jar has to be specified
(setq org-plantuml-jar-path
      (expand-file-name "/opt/Homebrew/opt/plantuml/libexec/plantuml.jar"))

;; babel accept plantuml
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

;; Integration with org-mode
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1) ; 只需敲 1 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t) ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))) ; 根据选择的频率进行排序，读者如果不喜欢可以去掉

(use-package company-jedi
  :init
  (add-to-list 'company-backends 'company-jedi))

(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))

;; TabNine: AI 自动补全工具.  NOTE: 会导致 CPU 占用过高，引起“死机”。
; (use-package company-tabnine
;   :ensure t
;   :init
;   (add-to-list 'company-backends #'company-tabnine)
;   :config
;    ;; Trigger completion immediately.
;    (setq company-idle-delay 0)
;    ;; Number the candidates (use M-1, M-2 etc to select completions).
;    (setq company-show-numbers t)
;    ;; workaround for company-transformers
;    (setq company-tabnine--disable-next-transform nil)
;    (defun my-company--transform-candidates (func &rest args)
;      (if (not company-tabnine--disable-next-transform)
;          (apply func args)
;        (setq company-tabnine--disable-next-transform nil)
;        (car args)))
; 
;    (defun my-company-tabnine (func &rest args)
;      (when (eq (car args) 'candidates)
;        (setq company-tabnine--disable-next-transform t))
;      (apply func args))
; 
;    (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
;    (advice-add #'company-tabnine :around #'my-company-tabnine)
; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 代码扩展和自动补全
(use-package yasnippet
  :ensure t
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  ;; add company-yasnippet to company-backends
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
	backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  ;; unbind <TAB> completion
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil)
  :bind
  (:map yas-minor-mode-map ("S-<tab>" . yas-expand)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; ;; auto-complete
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-language)

;;; init-language.el ends here

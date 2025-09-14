;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Make Emacs Native-compile .elc files asynchronously by setting
;; `native-comp-jit-compilation' to t.
(setq native-comp-jit-compilation t)
(setq native-comp-deferred-compilation native-comp-jit-compilation)  ; Deprecated
(native-comp-available-p)

;; 启动的时候先不加载各种 package，等真需要的时候再加
(setq package-enable-at-startup nil)

;; If an `.el' file is newer than its corresponding `.elc', load the `.el'.
(setq load-prefer-newer t)


;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold most-positive-fixnum)
;(setq gc-cons-threshold (* 2 1000 1000))

;; Restore to normal value after startup (e.g. 5GB)
(add-hook 'emacs-startup-hook
        (lambda () (setq gc-cons-threshold (* 5000 1024 1024))))

;; Write any customizations to a temp file so they are discarded.
(setq custom-file (make-temp-file "custom-" nil ".el"))

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(scroll-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Give the frame basic coloring while waiting for the theme to load. The main purpose of this is to not blind me when it's dark by flashing a screen full of white. These colors are from doom-one.
(set-face-attribute 'default nil :background "#282c34" :foreground "#bbc2cf")

;; Resizing the Emacs frame can be a terribly expensive part of changing the font. By inhibiting this, we easily halve startup times with fonts that are larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

 ;; Ignore X resources; its settings would be redundant with the other settings in this file and can conflict with later config (particularly where the cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; 让 Emacs 停顿少一点
(setq redisplay-dont-pause t)

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here

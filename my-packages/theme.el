;;; theme.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package doom-themes
  :ensure t
  :custom-face
  (font-lock-doc-face ((t (:foreground "#9099ab"))))
  (font-lock-string-face ((t (:foreground "#ebcb8b"))))
  (font-lock-constant-face ((t (:foreground "#81a1c1"))))
  (font-lock-warning-face ((t (:foreground "#d08770"))))
  (font-lock-function-name-face ((t (:foreground unspecified :weight regular))))
  (font-lock-variable-name-face ((t (:foreground unspecified :weight regular))))
  (font-lock-builtin-face ((t (:foreground "#81a1c1"))))
  (font-lock-type-face ((t (:foreground unspecified))))
  (font-lock-keyword-face ((t (:foreground "#81a1c1"))))
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
        doom-themes-enable-italic nil) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t))

(use-package font-lock
  :custom-face
  ;; (font-lock-keyword-face ((t (:foreground unspecified))))
  ;; (font-lock-operator-face ((t (:foreground unspecified))))
  ;; (font-lock-type-face ((t (:foreground unspecified))))
  ;; (font-lock-variable-name-face ((t (:foreground unspecified))))
  ;; (font-lock-constant-face ((t (:foreground unspecified))))
  ;; (font-lock-number-face ((t (:foreground unspecified))))
  ;; (font-lock-string-face ((t (:foreground "#c78552"))))
  ;; (font-lock-function-name-face ((t (:foreground unspecified :weight normal))))
  ;; (font-lock-doc-face ((t (:foreground unspecified :inherit 'font-lock-comment-face))))
  ;; (font-lock-preprocessor-face ((t (:foreground unspecified))))
  ;; (font-lock-builtin-face ((t (:foreground unspecified))))
  (web-mode-function-call-face ((t (:foreground unspecified :inherit nil :weight normal)))))

(provide 'theme)

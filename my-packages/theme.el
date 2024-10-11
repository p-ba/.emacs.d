(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t)
  )

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

(set-face-attribute 'mode-line nil
                    :background 'unspecified
                    :box t)

(set-face-attribute 'fringe nil
                      :background 'unspecified)

(provide 'theme)

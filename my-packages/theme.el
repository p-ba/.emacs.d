;; (use-package organic-green-theme
;;   :ensure t
;;   :custom-face
;;   (font-lock-keyword-face ((t (:foreground "#008888"))))
;;   :config
;;   (load-theme 'organic-green t)
;;   (set-face-attribute 'fringe nil :background 'unspecified)
;;   (set-face-attribute 'mode-line nil :background 'unspecified :box t))

;; (use-package ef-themes
;;   :ensure t
;;   :config
;;   (load-theme 'ef-maris-light t))

;; (use-package nano
;;   :vc (:url "https://github.com/rougier/nano-emacs" :branch "master")
;;   :init
;;   (require 'nano-layout)
;;   (require 'nano-faces)
;;   (require 'nano-theme)
;;   (tool-bar-mode -1)
;;   (scroll-bar-mode -1)
;;   (require 'nano-theme-dark)
;;   (require 'nano-theme-light)
;;   (nano-theme-set-dark)
;;   (require 'nano-session)
;;   (require 'nano-modeline)
;;   (require 'nano-bindings)
;;   :config
;;   (call-interactively 'nano-refresh-theme)
;;   (custom-set-faces
;;    '(default ((t (:family "Menlo" :foundry "nil" :slant normal :weight regular :height 180 :width normal))))))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package auto-dark
;;   :ensure t
;;   :custom
;;   (auto-dark-themes '((ef-owl) (ef-maris-light)))
;;   (auto-dark-polling-interval-seconds 30)
;;   (auto-dark-allow-osascript t)
;;   :init (auto-dark-mode))

;; (use-package catppuccin-theme
;;   :ensure t
;;   :custom-face
;;   (font-lock-builtin-face ((t (:foreground "#8caaee"))))
;;   (font-lock-keyword-face ((t (:foreground "#5671b0"))))
;;   (font-lock-type-face ((t (:foreground "#b49bf2"))))
;;   (font-lock-string-face ((t (:foreground "#9eba8c" :weight normal))))
;;   (font-lock-preprocessor-face ((t (:foreground "#737994"))))
;;   (font-lock-constant-face ((t (:foreground "#de9bf2" :inherit 'font-lock-type-face))))
;;   :config
;;   (setq catppuccin-flavor 'frappe)
;;   (load-theme 'catppuccin :no-confirm)
;;   (set-face-attribute 'fringe nil
;;                       :background 'unspecified)
;;   (set-face-attribute 'mode-line nil
;;                       :background 'unspecified
;;                       :box t))

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

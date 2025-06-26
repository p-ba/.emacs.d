;; (ido-mode)

(setq completion-auto-help 'always)
(setq ido-use-filename-at-point 'guess)

(use-package fzf-native
  :ensure t
  :vc (:url "https://github.com/dangduc/fzf-native" :branch "main")
  :config
  (fzf-native-load-dyn)
  (setq fussy-score-fn 'fussy-fzf-native-score))

(use-package fussy
  :ensure t
  :config
  (fussy-setup)
  (fussy-eglot-setup))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))

(use-package company
    :defer 0.1
    :config
    (global-company-mode t)
    (setq-default
        company-idle-delay 0.05
        company-require-match nil
        company-minimum-prefix-length 0
        company-frontends '(company-preview-frontend)
        ;; company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)
        ))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(provide 'autocomplete)

(use-package corfu
  :ensure t
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :custom
  (text-mode-ispell-word-completion nil)
  (corfu-echo-documentaion 0.0)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package consult
  :custom
  (global-set-key (kbd "s-b") 'consult-bookmark)
  (consult-async-min-input 1)
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-enable-symbol-highlighting nil)
  (lsp-completion-provider :none)
  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-diagnostics-provider :none)
  (lsp-enable-file-watchers nil)
  (lsp-ui-sideline-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  :ensure t)

(provide 'autocomplete)

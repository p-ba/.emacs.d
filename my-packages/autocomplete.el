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
  (consult-async-min-input 1)
  :bind
  ("s-b" . consult-bookmark)
  ("C-x b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-x p b" . consult-project-buffer)
  ("C-x C-r" . consult-recent-file)
  ("C-x f" . find-file)
  ("C-x p g" . consult-ripgrep)
  :ensure t)

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-collect)))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-keep-workspace-alive nil)
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

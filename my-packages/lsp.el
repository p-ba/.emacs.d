(defun lazy-setup ()
  (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend))

(use-package eglot
  :defer t
  :hook ((eglot-managed-mode . lazy-setup))
  :custom
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  :config
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio"))
  (add-to-list 'eglot-server-programs '(typescript-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(js-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(tsx-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(jsx-mode "typescript-language-server" "--stdio")))

(use-package eglot-booster
  :after eglot
  :ensure t
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :branch "main")
  :config (eglot-booster-mode))

(provide 'lsp)

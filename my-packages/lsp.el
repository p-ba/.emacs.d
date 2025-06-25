(defun lazy-setup ()
  (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend))

(use-package eglot
  :defer t
  :hook ((eglot-managed-mode . lazy-setup))
  :custom
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  :config
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio")))

(provide 'lsp)

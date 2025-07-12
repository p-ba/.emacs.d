;;; lsp.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(defun lazy-setup ()
  (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend))

(setq vue-ts-options #s(hash-table test equal data
                                   ("plugins"
                                    [#s(hash-table test equal data
                                                   ("name" "@vue/typescript-plugin"
                                                    "location" ""
                                                    "languages" ["vue"]))])))

(use-package eglot
  :defer t
  :hook ((eglot-managed-mode . lazy-setup))
  :custom
  (eglot-connect-hook nil)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider :inlayHints))
  :config
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-server-programs `(vue-mode . ("typescript-language-server" "--stdio" :initializationOptions ,vue-ts-options)))
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

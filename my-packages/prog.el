;;; prog.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'rg))

(use-package web-mode
  :ensure t
  :config
  (define-derived-mode vue-mode web-mode "Vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode)))

(defun detect-web-mode()
  (setq now (point))
  (goto-line 1)
  (when (search-forward "?>" nil t)
	(web-mode))
  (goto-char now))

(use-package php-ts-mode
  :mode (("\\.php$" . php-ts-mode))
  :hook (php-ts-mode . detect-web-mode))

(use-package go-ts-mode)

(use-package yaml-ts-mode)

(use-package typescript-ts-mode)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . gfm-mode))
  :commands gfm-mode
  :custom (markdown-command "pandoc --standalone --mathjax --from=markdown"))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package highlight-indent-guides
  :ensure t
  :hook ((yaml-ts-mode . highlight-indent-guides-mode)))

(provide 'prog)

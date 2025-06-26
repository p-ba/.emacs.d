(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'rg))

(use-package web-mode
  :ensure t)

(defun detect-web-mode()
  (setq now (point))
  (goto-line 1)
  (if (search-forward "?>" nil t)
	  (web-mode)
    (php-ts-mode))
  (goto-char now))

(use-package typescript-mode
  :ensure t)

(add-hook 'prog-mode-hook #'editorconfig-apply)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

(use-package php-mode
  :ensure t
  :hook (php-mode . detect-web-mode))

(use-package groovy-mode
  :ensure t)

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

(use-package aider
  :ensure t)

(provide 'prog)

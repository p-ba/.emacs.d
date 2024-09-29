(unless (package-installed-p 'php-ts-mode)
  (package-vc-install "https://github.com/emacs-php/php-ts-mode"))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'rg))

(defun prog-mode-config()
  (setq display-line-numbers-type 'relative)
  (display-line-numbers-mode))
(add-hook 'prog-mode-hook 'prog-mode-config)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'all)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (add-to-list 'treesit-auto-langs 'php)
  (global-treesit-auto-mode))

(use-package web-mode
  :ensure t)

(defun detect-web-mode()
  (setq now (point))
  (goto-line 1)
  (when (search-forward "?>" nil t)
	(web-mode))
  (goto-char now))

(use-package php-mode
  :ensure t
  :hook (php-mode . detect-web-mode))

(use-package php-ts-mode
  :hook (php-ts-mode . detect-web-mode))

(use-package php-find-use
  :config
  (global-set-key (kbd "C-c f u") 'php-find-use))

(provide 'prog)

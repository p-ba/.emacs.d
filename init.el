(add-to-list 'load-path (concat user-emacs-directory "my-packages/"))

(electric-indent-mode 1)
(global-auto-revert-mode 1)

(setq
 use-short-answers t
 y-or-n-p-use-read-key t 
 ring-bell-function 'ignore
 scroll-preserve-screen-position t
 inhibit-startup-screen t
 custom-file "~/.emacs.d/init-custom.el"
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 auto-save-file-name-transforms `((".*" "~/.emacs.d/saves" t)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-native-compile t
      native-comp-async-report-warnings-errors nil)
(package-initialize)

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'super)
(global-set-key (kbd "s-SPC") 'set-mark-command)
(setq superword-mode 1)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(unless (package-installed-p 'php-ts-mode)
  (package-vc-install "https://github.com/emacs-php/php-ts-mode"))

(use-package emacs
  :config
  (set-face-attribute 'default nil :font "Iosevka Comfy" :height 170))

(use-package organic-green-theme
  :init (progn (load-theme 'organic-green t)
               (enable-theme 'organic-green))
  :defer t
  :ensure t)

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'rg))

(defun prog-mode-init ()
  (setq display-line-numbers-type 'relative)
  (display-line-numbers-mode)
  (setq tab-width 4))
(add-hook 'prog-mode-hook 'prog-mode-init)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'all)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (add-to-list 'treesit-auto-langs 'php)
  (global-treesit-auto-mode))

(use-package rg
  :ensure t
  :config
  (rg-define-search rg-all
	:dir project
	:flags ("--no-ignore")))

(use-package web-mode
  :ensure t)

(defun detect-web-mode()
  (setq now (point))
  (goto-line 1)
  (when (search-forward "?>" nil t)
	(web-mode))
  (goto-char now))

(use-package php-mode
  :config
  (add-hook 'php-mode-hook 'detect-web-mode)
  :ensure t)

(use-package php-ts-mode
  :config
  (add-hook 'php-ts-mode-hook 'detect-web-mode))

(require 'php-find-use)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (defun forward-evil-word (&optional count)
	"Overrides 'forward-evil-word' from 'evil-common.el'
     Respects superword-mode setting"
	(if (bound-and-true-p superword-mode)
		(forward-evil-symbol count)
	  (evil-forward-nearest
	   count
	   #'(lambda (&optional cnt)
		   (let ((word-separating-categories evil-cjk-word-separating-categories)
				 (word-combining-categories evil-cjk-word-combining-categories)
				 (pnt (point)))
			 (forward-word cnt)
			 (if (= pnt (point)) cnt 0)))
	   #'(lambda (&optional cnt)
		   (evil-forward-chars "^[:word:]\n\r\t\f " cnt))
	   #'forward-evil-empty-line)))
  (setq-default evil-symbol-word-search t)

  (global-unset-key (kbd "C-u"))
  (evil-define-key 'normal 'global (kbd "C-u") 'scroll-down)
  
  (evil-set-leader nil (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>p") 'project-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader>f") 'project-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>g") 'rg-project)
  (evil-define-key 'normal 'global (kbd "<leader>G") 'rg-all)
  (evil-define-key 'normal 'global (kbd "<leader>r") 'consult-imenu)
  (evil-define-key 'normal 'global (kbd "<leader>b") 'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>u") 'universal-argument)
  (evil-define-key 'normal 'global (kbd "<leader>s") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>i") 'php-find-use)

  (dolist (initial-states '((xref--xref-buffer-mode . emacs)
							(occur-mode . emacs)
							(vterm-mode . emacs)
							(shell-mode . emacs)
							(rg-mode . emacs)
							(diff-mode . emacs)))
	(evil-set-initial-state (car initial-states) (cdr initial-states))))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (setq dabbrev-case-fold-search nil
		dabbrev-case-replace nil))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

(use-package vterm
  :ensure t
  :ensure t)

(use-package multi-vterm
  :ensure t
  :after vterm)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package wgrep
  :ensure t)

(load custom-file 'noerror 'nomessage)

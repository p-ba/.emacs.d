(set-face-attribute 'default nil :font "Iosevka Comfy" :height 170)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(electric-indent-mode 1)
;; (electric-pair-mode 1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(global-auto-revert-mode 1)
(setq custom-file "~/.emacs.d/init-custom.el")
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/saves" t)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq required-packages
     '(markdown-mode
	  consult
      vertico
      magit
      editorconfig-generate
      editorconfig
      undo-tree
      web-mode
      evil
      vue-mode
      vue-html-mode
      php-mode
      dumb-jump
      corfu
	  rg
      treesit-auto
      tree-sitter-langs))

(setq required-vc-packages
      '((php-ts-mode "https://github.com/emacs-php/php-ts-mode")))

(dolist (val required-packages)
  (unless (package-installed-p val)
    (package-install val)))

(dolist (pval required-vc-packages)
  (unless (package-installed-p (nth 0 pval))
    (package-vc-install (nth 1 pval))))

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

(use-package organic-green-theme
  :init (progn (load-theme 'organic-green t)
               (enable-theme 'organic-green))
  :defer t
  :ensure t)

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'rg))

(defun prog-mode-init ()
  (setq display-line-numbers-type 'relative)
  (display-line-numbers-mode)
  (setq tab-width 4))
(add-hook 'prog-mode-hook 'prog-mode-init)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (add-to-list 'major-mode-remap-alist '(php-mode . php-ts-mode))
  (add-to-list 'major-mode-remap-alist '(php-mode-maybe . php-ts-mode))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package rg
  :config
  (rg-define-search rg-all
	:dir project
	:flags ("--no-ignore")))

(use-package php-find-use
  :load-path "my-packages/")

(use-package evil
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
  (evil-define-key 'normal 'global (kbd "<leader>f") 'consult-fd)
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
  :init
  (global-corfu-mode))

(use-package vterm
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

(load custom-file 'noerror 'nomessage)
(put 'erase-buffer 'disabled nil)

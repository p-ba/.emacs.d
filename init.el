(add-to-list 'load-path (concat user-emacs-directory "my-packages/"))
(require 'unfuck)

(require 'package)
(setq package-native-compile t
	  package-archives '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("devel" . "https://elpa.gnu.org/devel/"))
      package-archive-priorities '(("devel" . -1))
      native-comp-async-report-warnings-errors nil)

(require 'theme)

(global-set-key (kbd "s-;") 'pop-to-mark-command)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(unless (package-installed-p 'php-ts-mode)
  (package-vc-install "https://github.com/emacs-php/php-ts-mode"))

(use-package ace-jump-mode
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'ace-jump-mode))

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
  :ensure t
  :hook (php-mode . detect-web-mode))

(use-package php-ts-mode
  :hook (php-ts-mode . detect-web-mode))

(use-package php-find-use
  :config
  (global-set-key (kbd "C-c f u") 'php-find-use))

(use-package lsp-mode
  :custom
  (lsp-enable-symbol-highlighting nil)
  (lsp-completion-provider :none)
  (lsp-enable-file-watchers nil)
  (lsp-diagnostics-mode -1)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  :ensure t)

(defvar-keymap leader-commands
  :doc "Commands for projects"
  "f" 'project-find-file
  "p" 'project-switch-project
  "g" 'rg-projectsd
  "r" 'consult-imenu
  "G" 'rg-all
  "u" 'php-find-use)

(keymap-set global-map "C-q" leader-commands)

(defun toggle-evil()
  (interactive)
  (if evil-mode
	  ((global-unset-key (kbd "C-u"))
	   (evil-define-key 'normal 'global (kbd "<leader>-u") 'unversal-argument)
	   (evil-define-key 'normal 'global (kbd "C-u") 'scroll-down))
	(global-set-key (kbd "C-u") 'universal-argument)
  ))

(setq evil-want-keybinding nil)
(use-package evil
  :ensure t
  :hook (evil-mode . (call-interactively toggle-evil))
  :config
  ;; (evil-mode 1)
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

  (global-set-key (kbd "M-<down>") 'evil-scroll-down)
  (global-set-key (kbd "M-<up>") 'evil-scroll-up)

  (evil-set-leader nil (kbd "SPC"))
  (dolist (keymap leader-commands)
	(when (consp keymap)
	  (setq key (car keymap))
	  (when key
		(evil-define-key 'normal 'global (kbd (format "<leader>%c" key)) (cdr keymap))))))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (if evil-mode
	  (evil-collection-init)))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (setq dabbrev-case-fold-search nil
		dabbrev-case-replace nil)
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package corfu
  :ensure t
  :custom
  (text-mode-ispell-word-completion nil)
  :init
  (global-corfu-mode))

(use-package consult
  :custom
  (consult-async-min-input 1)
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package magit
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package wgrep
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . gfm-mode))
  :commands gfm-mode
  :bind (:map markdown-mode-map ("C-c l" . slot/often-used-links))
  :custom (markdown-command "pandoc --standalone --mathjax --from=markdown"))

(use-package which-key
  :config
  (which-key-mode))

(load custom-file 'noerror 'nomessage)

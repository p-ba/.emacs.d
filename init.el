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
(require 'prog)

(global-set-key (kbd "s-;") 'pop-to-mark-command)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(defun my/scroll-down()
  (interactive)
  (scroll-down -10))

(defun my/scroll-up()
  (interactive)
  (scroll-down 10))

(global-set-key (kbd "M-<down>") 'my/scroll-down)
(global-set-key (kbd "M-<up>") 'my/scroll-up)

(use-package ace-jump-mode
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'ace-jump-mode))

(use-package rg
  :ensure t
  :config
  (rg-define-search rg-all
	:dir project
	:flags ("--no-ignore")))

(use-package lsp-mode
  :custom
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

(defvar-keymap leader-commands
  :doc "Commands for projects"
  "f" 'project-find-file
  "p" 'project-switch-project
  "g" 'rg-project
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
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-history-directory-alist '(("." . (expand-file-name "backups/" user-emacs-directory))))
  :config
  (global-undo-tree-mode 1))

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

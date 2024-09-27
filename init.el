(add-to-list 'load-path (concat user-emacs-directory "my-packages/"))

(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(electric-indent-mode 1)
(global-auto-revert-mode 1)
(global-superword-mode t)

(setq-default
  cursor-in-non-selected-windows nil)

(setq
 use-short-answers t
 y-or-n-p-use-read-key t
 ring-bell-function 'ignore
 scroll-preserve-screen-position t
 inhibit-startup-screen t
 kill-whole-line 1
 custom-file "~/.emacs.d/init-custom.el"
 save-files-directory (expand-file-name "backups/" user-emacs-directory)
 auto-save-file-name-transforms `((".*" ,save-files-directory t))
 backup-directory-alist `(("." . ,save-files-directory)))

(require 'package)
(setq package-native-compile t
	  package-archives '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("devel" . "https://elpa.gnu.org/devel/"))
      package-archive-priorities '(("devel" . -1))
      native-comp-async-report-warnings-errors nil)

(use-package nano
  :ensure t
  :vc (:url "https://github.com/rougier/nano-emacs" :branch "master"))

;; OSX-specific
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'super)
(global-set-key (kbd "s-;") 'pop-to-mark-command)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(unless (package-installed-p 'php-ts-mode)
  (package-vc-install "https://github.com/emacs-php/php-ts-mode"))

(use-package emacs
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  (set-mark-command-repeat-pop t)
  (enable-recursive-minibuffers t)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (setq require-final-newline t)
  (set-face-attribute 'default nil :font "Iosevka Comfy" :height 170)
  (advice-add 'pop-to-mark-command :around
            (lambda (pop-to-mark &rest args)
              (let ((p (point)))
                (dotimes (_ 5)
                  (when (= p (point))
                    (apply pop-to-mark args)))))))

(use-package ace-jump-mode
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'ace-jump-mode))

(use-package dired
  :custom
  (setq insert-directory-program "gls")
  (dired-use-ls-dired t)
  (dired-recursive-copies 'always)
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-Alhv --group-directories-first"))

;; (use-package organic-green-theme
;;   :init (progn (load-theme 'organic-green t)
;;                (enable-theme 'organic-green))
;;   :defer t
;;   :ensure t)

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'rg))

(defun prog-mode-config()
  (setq display-line-numbers-type 'relative)
  (display-line-numbers-mode)
  (setq tab-width 4)
  (indent-tabs-mode  nil))
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
  :config
  (add-hook 'php-mode-hook 'detect-web-mode)
  :ensure t)

(use-package php-ts-mode
  :config
  (add-hook 'php-ts-mode-hook 'detect-web-mode))

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
  "g" 'rg-project
  "r" 'consult-imenu
  "G" 'rg-all
  "u" 'php-find-use)

(keymap-set global-map "C-q" leader-commands)

(setq evil-want-keybinding nil)
(use-package evil
  :ensure t
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

  (global-unset-key (kbd "C-u"))
  (evil-define-key 'normal 'global (kbd "C-u") 'scroll-down)
  ;; (use-package evil-collection
  ;; 	:ensure t
  ;; 	:config
  ;; 	(evil-collection-init))

  (evil-set-leader nil (kbd "SPC"))
  (dolist (keymap leader-commands)
	(when (consp keymap)
	  (setq key (car keymap))
	  (when key
		(evil-define-key 'normal 'global (kbd (format "<leader>%c" key)) (cdr keymap))))))

;;   ;; (dolist (initial-states '((xref--xref-buffer-mode . emacs)
  ;; 							(occur-mode . emacs)
  ;; 							(shell-mode . emacs)
  ;; 							(rg-mode . emacs)
  ;; 							(diff-mode . emacs)))
  ;; 	(evil-set-initial-state (car initial-states) (cdr initial-states))))



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

(load custom-file 'noerror 'nomessage)

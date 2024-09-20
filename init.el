(load-theme 'kuronami t)
(set-face-attribute 'default nil :height 150)
(tool-bar-mode 0)
(setq inhibit-startup-screen t)
(setq custom-file "~/.emacs.d/init-custom.el")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq required-packages
     '(markdown-mode
      vertico
      magit
      editorconfig-generate
      editorconfig
      undo-tree
      web-mode
      evil
      kuronami-theme
      vue-mode
      vue-html-mode
      php-mode
      dumb-jump
      expand-region
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

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/saves/" t)))
(setq tab-width 4)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'super)
(global-set-key (kbd "s-SPC") 'set-mark-command)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq dumb-jump-force-searcher 'rg)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package evil
  :config
  (evil-set-initial-state 'grep-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'xref-etags-mode 'emacs)
  (evil-define-key 'normal 'global (kbd "<leader>f") 'project-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>g") 'project-find-regexp)
  (evil-define-key 'normal 'global (kbd "<leader>p") 'project-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader>b") 'project-switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>u") 'universal-argument)
  (evil-set-leader nil (kbd "<SPC>") nil)
  (global-unset-key (kbd "C-u"))
  (evil-define-key 'normal 'global (kbd "C-u") 'scroll-down)
  (evil-mode 1))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(vertico-mode)

(editorconfig-mode 1)

(load custom-file)

(add-to-list 'load-path (concat user-emacs-directory "my-packages/"))
(require 'unfuck)
(require 'theme)
(require 'prog)
(require 'autocomplete)

(global-set-key (kbd "s-;") 'pop-to-mark-command)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

;; kill and copy whole line when no region is active
(defun slick-cut (beg end &optional arg)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-region :before #'slick-cut)

(defun slick-copy (beg end &optional arg)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-ring-save :before #'slick-copy)
;; endof kill and copy whole line when no region is active

(defun my/scroll-down()
  (interactive)
  (scroll-down -15))

(defun my/scroll-up()
  (interactive)
  (scroll-down 15))

(global-set-key (kbd "M-<down>") 'my/scroll-down)
(global-set-key (kbd "M-<up>") 'my/scroll-up)

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char-timer))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

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

  (evil-set-leader nil (kbd "SPC")))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (if evil-mode
	  (evil-collection-init)))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package recentf
  :config
  (recentf-mode 1)
  (global-set-key (kbd "C-x C-r") 'recentf))

(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
  :config
  (global-undo-tree-mode 1))

(use-package magit
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package wgrep
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(load custom-file 'noerror 'nomessage)

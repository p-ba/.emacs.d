;;; init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
(use-package compile-angel
  :ensure t
  :demand t
  :custom
  (compile-angel-verbose t)
  :config
  (push "/init.el" compile-angel-excluded-files)
  (push "/init-custom.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (compile-angel-on-load-mode))

(add-to-list 'load-path (concat user-emacs-directory "my-packages/"))
(require 'unfuck)
(require 'theme)
(require 'projectd)
(require 'prog)
(require 'autocomplete)
(require 'dumb-import)
(require 'lsp)

(defun adjust-font-height (delta)
  (let* ((current-height (face-attribute 'default :height))
         (new-height (+ current-height delta)))
    (set-face-attribute 'default nil :height new-height)))

(global-set-key (kbd "M-=") (lambda ()
                              (interactive)
                              (adjust-font-height 10)))
(global-set-key (kbd "M--") (lambda ()
                              (interactive)
                              (adjust-font-height -10)))
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

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

(defun my/copy-file-path-to-clipboard ()
  "Copy current file path to clipboard.
   If inside a project, copy relative path to project root; otherwise, copy absolute path."
  (interactive)
  (let* ((file-path (buffer-file-name))
         (project-root-path (when (project-current)
                                (project-root (project-current))))
         (path-to-copy nil))
    (cond
     ((null file-path)
      (message "No file is being visited in this buffer."))
     ((and project-root-path (file-in-directory-p file-path project-root-path))
      ;; File is in a project, copy relative path
      (setq path-to-copy (file-relative-name file-path project-root-path)))
     (t
      ;; Not in a project or file not within project root, copy absolute path
      (setq path-to-copy file-path)))

    (when path-to-copy
      (message path-to-copy)
      (kill-new path-to-copy)
      (message "Copied: %s" path-to-copy))))

(global-set-key (kbd "C-c C-c") 'my/copy-file-path-to-clipboard)

(defun my/nav-project-switch-project (dir)
  (interactive (list (project-prompt-project-dir)))
  (let ((project-current-directory-override dir))
    (print project-current-directory-override)
    (project-find-file)))

(global-set-key (kbd "C-x p p") 'my/nav-project-switch-project)

(defun my/scroll-down()
  (interactive)
  (next-line 15)
  (recenter))

(defun my/scroll-up()
  (interactive)
  (previous-line 15)
  (recenter))

(global-set-key (kbd "C-g") 'keyboard-quit)

(global-set-key (kbd "M-<down>") 'my/scroll-down)
(global-set-key (kbd "M-<up>") 'my/scroll-up)

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(setopt grep-command "grep --color=auto -rni -A1 -B1 ")

(defun rgrep-project (arg)
  (interactive "p")
  (if buffer-file-truename
      (setq-local current-ext (car (nreverse (string-split buffer-file-truename "\\."))))
    (setq-local current-ext nil))
  (let* ((regex (read-string (format "Pattern, default: %s: " (current-word)) nil nil (current-word)))
         (ext (read-string (format "Extenions, current: %s: " current-ext) nil nil current-ext))
         (project (project-current)))
    (if (< 0 (length ext))
        (setq grep-find-template (format "fd -t f -e %s -X grep --color=auto -nHi -e '%s'" ext regex))
      (setq grep-find-template (format "fd -t f -X grep --color=auto -nHi -e '%s'" regex)))
    (if (string-equal "*" ext)
        (setq grep-find-template (format "fd -t f -X grep --color=auto -nHi -e '%s'" regex)))
    (if project
        (rgrep grep-find-template ext (project-root project))
      (if default-directory
          (rgrep grep-find-template ext default-directory)
        (print "No directory selected")))))

(global-set-key (kbd "C-x p g") 'rgrep-project)

(when (daemonp)
  (use-package exec-path-from-shell
    :ensure t
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))
    (when (daemonp)
      (exec-path-from-shell-initialize))))

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package magit
  :ensure t)

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  :ensure t)

(load custom-file 'noerror 'nomessage)

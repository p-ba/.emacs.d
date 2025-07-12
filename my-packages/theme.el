;;; theme.el --- ui -*- no-byte-compile: t; lexical-binding: t; -*-

(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 200
                    :weight 'normal)

(defun git-branch-info ()
  "Return current Git branch if available."
  (when (fboundp 'magit-get-current-branch)
    (let ((branch (magit-get-current-branch)))
      (when branch
        (propertize branch 'face '(:foreground "light green"))))))

(defun current-file-display-name ()
  (let* ((file-path (buffer-file-name))
         (project-root-path (when (project-current)
                                (project-root (project-current))))
         (path-to-copy nil))
    (cond
     ((null file-path)
      path-to-copy "")
     ((and project-root-path (file-in-directory-p file-path project-root-path))
      ;; File is in a project, show relative path
      (setq path-to-copy (file-relative-name file-path project-root-path)))
     (t
      ;; Not in a project or file not within project root, show absolute path
      (setq path-to-copy buffer-file-truename)))

    path-to-copy))

(define-key mode-line-major-mode-keymap [header-line]
            (lookup-key mode-line-major-mode-keymap [mode-line]))

(defun mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))

(setq-default mode-line-format
     '((:eval
       (mode-line-render
       (format-mode-line (list
         (propertize " ☰" 'face `(:inherit mode-line-buffer-id)
                         'help-echo "Mode(s) menu"
                         'mouse-face 'mode-line-highlight
                         'local-map   mode-line-major-mode-keymap)
         " " (current-file-display-name) " "
         (if (and buffer-file-name (buffer-modified-p))
             (propertize "(modified)" 'face `(:inherit face-faded)))))
       (format-mode-line
        (propertize "%4l:%2c  " 'face `(:inherit face-faded)))))))

(setq default-frame-alist
      (append (list '(width  . 72) '(height . 40)
                    '(vertical-scroll-bars . nil)
                    '(internal-border-width . 24))))

(set-frame-parameter (selected-frame)
                     'internal-border-width 24)

(fringe-mode '(0 . 0))


(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package doom-themes
  :ensure t
  :custom-face
  (font-lock-doc-face ((t (:foreground "#9099ab"))))
  (font-lock-string-face ((t (:foreground "#ebcb8b"))))
  (font-lock-constant-face ((t (:foreground "#81a1c1"))))
  (font-lock-warning-face ((t (:foreground "#d08770"))))
  (font-lock-function-name-face ((t (:foreground unspecified :weight regular))))
  (font-lock-variable-name-face ((t (:foreground unspecified :weight regular))))
  (font-lock-builtin-face ((t (:foreground "#81a1c1"))))
  (font-lock-type-face ((t (:foreground unspecified))))
  (font-lock-keyword-face ((t (:foreground "#81a1c1"))))
  (web-mode-function-call-face ((t (:foreground unspecified :inherit nil :weight normal))))
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
        doom-themes-enable-italic nil) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t))

(provide 'theme)

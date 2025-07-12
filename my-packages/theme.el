;;; theme.el --- ui -*- no-byte-compile: t; lexical-binding: t; -*-

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

(setq-default mode-line-format
              '(" " (:eval (current-file-display-name)) ":%l:%c "
                "("
                (:eval mode-name)
                ")"
                " ["
                (:eval (git-branch-info))
                "]"
                ))

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

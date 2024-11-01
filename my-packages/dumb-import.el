(require 'async-completing-read)

(defun dumb-import/php-namespace-to-use (namespace classname)
  (format "%s\\%s" (substring (nth 1 (split-string namespace " ")) 0 -2) classname))

(setq dumb-import/php-grep-names
      '("class[[:space:]]*%NAME%[[:space:]]*$*"
        "trait[[:space:]]*%NAME%[[:space:]]*$*"
        "interface[[:space:]]*%NAME%[[:space:]]*$*"
        "function[[:space:]]*%NAME%[[:space:]]*\("))

(defun dumb-import/php-grep-args (name path)
  (setq-local grep-args `(, path))
  (dolist (grep-name dumb-import/php-grep-names)
    (add-to-list 'grep-args (string-replace "%NAME%" name grep-name))
    (push "-e" grep-args))
  (append '("-rn" "--include" "*.php") grep-args))

(defun dumb-import ()
  (interactive)
  (let ((pwd (if (project-current)
                 (project-root (project-current))
               default-directory))
        (symbol-name (read-string (format "Symbol name to import, default: %s: " (current-word)) nil nil (current-word))))
    (let* ((grep-result
            (async-completing-read
             "From: "
             (apply #'acr-lines-from-process "grep" (dumb-import/php-grep-args symbol-name pwd))))
           (result
            (dumb-import/php-namespace-to-use
             (shell-command-to-string (format "grep -ohe \'^namespace.*;$\' %s" (car (split-string grep-result ":"))))
             symbol-name)))
      (when result
        (let ((now (point))
              (flag nil))
          (goto-line 1)
          (while (setq-local new-point (re-search-forward "^use.*?;" nil t))
	        (setq flag new-point))
          (if flag
              (insert (format "\nuse %s;" result))
            (when (re-search-forward "^namespace.*?;" nil t)
              (previous-line)
              (end-of-line)
              (insert "\n")
              (insert (format "\nuse %s;\n" result))))
          (goto-char now))))))

(provide 'dumb-import)

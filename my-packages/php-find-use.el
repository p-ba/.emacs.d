(defun php-namespace-to-use(namespace classname)
  (message (nth 1 (split-string namespace " ")))
  (format "use %s\\%s;" (substring (nth 1 (split-string namespace " ")) 0 -1) classname))

(defun php-find-use()
  (interactive)

  ;; set search root
  (setq pwd default-directory)
  (when (project-current)
	(setq pwd (project-root (project-current))))
  (unless pwd
	(setq pwd "~/"))
  
  (setq default-search (current-word))
  (setq query (format "Symbol name to import, default %s:" default-search))
  (when (= (length default-search) 0)
	(setq query "Symbol name to import:"))
  (setq obj-name (read-string query))
  (when (= (length obj-name) 0)
	(setq obj-name default-search))
  
  (setq out (shell-command-to-string (format "rg -l --no-ignore --vimgrep -g \'*.{php}\' \'^(class|interface|trait)[\s]*%s[\s\n]\' \'%s\' | xargs grep -ohe \'^namespace.*;$\'" obj-name pwd)))
  (setq results '())
  (dolist (namespace (split-string out "\n"))
	(unless (= (length namespace) 0)
	  (add-to-list 'results (php-namespace-to-use namespace obj-name))))

  (setq result (completing-read-default "Select symbol to import: " results))
  (when result
	(setq now (point))
	(goto-line 1)
	(setq flag nil)
	(while (setq pos (re-search-forward "^use.*?;" nil t))
	  (setq flag pos))
	(unless flag
	  (setq flag (re-search-forward "^namespace.*?;" nil t)))
	(when flag
	  (insert (format "\n%s" result)))
	(goto-char now)))

(provide 'php-find-use)

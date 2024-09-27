(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist   nil
	  message-log-max           16384
	  gc-cons-threshold         402653184
	  gc-cons-percentage        0.6)
(add-hook 'after-init-hook
		  (lambda ()
			(setq file-name-handler-alist file-name-handler-alist-old
				  gc-cons-threshold 16777216
				  gc-cons-percentage 0.1)
			(garbage-collect))
		  t)

;; Always recompile libraries if needed.  This being in early init is in
;; line with `auto-compile's manual.
(let* ((dir "~/.emacs.d/elpa/")
       (packages (directory-files dir))
       (get-pkg (lambda (pkg)
                  (seq-find (lambda (p) (string-prefix-p pkg p))
                            packages))))
  (dolist (pkg (list (funcall get-pkg "compat")
                     (funcall get-pkg "packed")
                     (funcall get-pkg "auto-compile")))
    (add-to-list 'load-path (concat dir pkg))))
(setq load-prefer-newer t)
(unless (package-installed-p 'auto-compile)
  (package-refresh-contents)
  (package-install 'auto-compile))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(setq frame-inhibit-implied-resize t)

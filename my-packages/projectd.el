;;; projectd.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(with-eval-after-load "project"
  ;; Override default behaviour of project.el
  ;; Use predefined list of locations, instead of saving visited projects in a file
  (setq known-project-locations (let ((config-path (expand-file-name ".known-project-locations.el" user-emacs-directory)))
                                        (if (file-exists-p config-path)
                                            (with-temp-buffer
                                              (insert-file-contents config-path)
                                              (read (buffer-string)))
                                          (with-temp-buffer
                                            (insert "()")
                                            (write-file config-path nil)
                                            '()))))

  (setq project--list (let ((found-projects nil))
                        (dolist (root-dir known-project-locations)
                          (when (file-directory-p root-dir)
                            (let ((dirs (directory-files root-dir t "[^.]" t)))
                              (dolist (dir dirs)
                                (when (file-directory-p dir)
                                  (add-to-list 'found-projects `(,(format "%s/" dir))))))))
                          found-projects)))

(provide 'projectd)

;;; projectd.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(with-eval-after-load "project"
  (setq known-project-locations (let ((config-path (expand-file-name ".known-project-locations.el" user-emacs-directory)))
                                        (if (file-exists-p config-path)
                                            (with-temp-buffer
                                              (insert-file-contents config-path)
                                              (read (buffer-string)))
                                          (with-temp-buffer
                                            (insert "()")
                                            (write-file config-path nil)
                                            '()))))

  (defun projectd-swtich-to-project (&optional directory)
    "Override default behaviour of project.el
     Use predefined list of locations, instead of saving visited projects in a file"
    (interactive)
    (let* ((find-command (concat "find " (string-join known-project-locations " ") " -mindepth 1 -maxdepth 1 -type d"))  ; with find
           (project-list (split-string (shell-command-to-string find-command) "\n" t)))
      (let ((selected-project
             (completing-read
              "Switch to a project: "
              project-list
              nil nil)))
        (when (and selected-project (file-directory-p selected-project))
          (project-switch-project selected-project)))))

  (define-key project-prefix-map (kbd "p") #'projectd-swtich-to-project))

(provide 'projectd)

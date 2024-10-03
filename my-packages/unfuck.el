;; No startup  screen
(setq inhibit-startup-screen t)

;; No startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer
(setq initial-buffer-choice nil)

;; No frame title
(setq frame-title-format nil)

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
;; (setq pop-up-windows nil)

;; No empty line indicators
(setq indicate-empty-lines nil)

;; No cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;; Text mode is initial mode
;; (setq initial-major-mode 'text-mode)

;; Text mode is default major mode
;; (setq default-major-mode 'text-mode)

;; Moderate font lock
;; (setq font-lock-maximum-decoration nil)

;; No limit on font lock
;; (setq font-lock-maximum-size nil)

;; No line break space points
(setq auto-fill-mode nil)

;; Fill column at 120
(setq fill-column 120)

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; Completion style, see
;; gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
(setq completion-styles '(basic substring))

;; Use RET to open org-mode links, including those in quick-help.org
(setq org-return-follows-link t)

;; Mouse active in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; No scroll bars
(set-scroll-bar-mode nil)

;; No toolbar
(tool-bar-mode -1)

;; No menu bar
(if (display-graphic-p)
    (menu-bar-mode t) ;; When nil, focus problem on OSX
  (menu-bar-mode -1))

;; Tab behavior
;; (setq tab-always-indent 'complete)
;; (global-company-mode)
;; (define-key company-mode-map [remap indent-for-tab-command]
;;   #'company-indent-or-complete-common)

;; Pixel scroll (as opposed to char scrool)
;; (pixel-scroll-mode t)

;; Mac specific
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t
        mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-use-title-bar nil))

;; Make sure clipboard works properly in tty mode on OSX
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(when (and (not (display-graphic-p))
           (eq system-type 'darwin))
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx))

;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Tab.space equivalence
(setq-default tab-width 4)

;; Auto-indent
(electric-indent-mode 1)

;; Better jumps
(global-superword-mode t)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Default shell in term
(unless
    (or (eq system-type 'windows-nt)
        (not (file-exists-p "/bin/zsh")))
  (setq-default shell-file-name "/bin/zsh")
  (setq explicit-shell-file-name "/bin/zsh"))

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Kill term buffer when exiting
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Start maximized
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(global-auto-revert-mode 1)

(setq
 ring-bell-function 'ignore
 scroll-preserve-screen-position t
 kill-whole-line 1
 backup-by-copying 1
 custom-file (expand-file-name "init-custom.el" user-emacs-directory)
 save-files-directory (expand-file-name "backups/" user-emacs-directory)
 auto-save-file-name-transforms `((".*" ,save-files-directory t))
 backup-directory-alist `(("." . ,save-files-directory)))

(use-package emacs
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  (set-mark-command-repeat-pop t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p) ;; do not show commands that are unavailabe in current mode in M-x results
  :config
  (setq require-final-newline t)
  (advice-add 'pop-to-mark-command :around
            (lambda (pop-to-mark &rest args)
              (let ((p (point)))
                (dotimes (_ 5)
                  (when (= p (point))
                    (apply pop-to-mark args)))))))

(use-package dired
  :custom
  (setq insert-directory-program "gls")
  (dired-use-ls-dired t)
  (dired-recursive-copies 'always)
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-Alhv --group-directories-first"))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (setq dabbrev-case-fold-search nil
		dabbrev-case-replace nil)
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package dired
  :config
  (setf dired-kill-when-opening-new-dired-buffer t))

;; (desktop-save-mode 1)

(provide 'unfuck)

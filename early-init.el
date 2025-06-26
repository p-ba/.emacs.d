(defvar minimal-emacs--backup-gc-cons-threshold gc-cons-threshold
  "Backup of the original value of `gc-cons-threshold' before startup.")

(setq gc-cons-threshold most-positive-fixnum)

(defvar emacs-gc-cons-threshold (* 32 1024 1024))

(setq garbage-collection-messages nil)

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil)

(defun after-init()
  (setq file-name-handler-alist file-name-handler-alist-old)
  (setq	gc-cons-threshold emacs-gc-cons-threshold))

(add-hook 'after-init-hook 'after-init)

(setq inhibit-compacting-font-caches t)

(when (and (not (daemonp)) (not noninteractive))
  ;; Resizing the Emacs frame can be costly when changing the font. Disable this
  ;; to improve startup times with fonts larger than the system default.
  (setq frame-resize-pixelwise t)

  ;; Without this, Emacs will try to resize itself to a specific column size
  (setq frame-inhibit-implied-resize t)

  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  (setq auto-mode-case-fold nil)

  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
  ;; dashboard) is more than enough, and faster to display.
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name)
  (setq initial-buffer-choice nil
        inhibit-startup-buffer-menu t
        inhibit-x-resources t)

  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Give up some bidirectional functionality for slightly faster re-display.
  (setq bidi-inhibit-bpa t)

  ;; Remove "For information about GNU Emacs..." message at startup
  (advice-add 'display-startup-echo-area-message :override #'ignore)

  ;; Suppress the vanilla startup screen completely. We've disabled it with
  ;; `inhibit-startup-screen', but it would still initialize anyway.
  (advice-add 'display-startup-screen :override #'ignore)

  ;; The initial buffer is created during startup even in non-interactive
  ;; sessions, and its major mode is fully initialized. Modes like `text-mode',
  ;; `org-mode', or even the default `lisp-interaction-mode' load extra packages
  ;; and run hooks, which can slow down startup.
  ;;
  ;; Using `fundamental-mode' for the initial buffer to avoid unnecessary
  ;; startup overhead.
  (setq initial-major-mode 'fundamental-mode
        initial-scratch-message nil)

  ;; Unset command line options irrelevant to the current OS. These options
  ;; are still processed by `command-line-1` but have no effect.
  (unless (eq system-type 'darwin)
    (setq command-line-ns-option-alist nil))
  (unless (memq initial-window-system '(x pgtk))
    (setq command-line-x-option-alist nil)))

(setq inhibit-splash-screen t)

(setq package-archives '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("devel" . "https://elpa.gnu.org/devel/"))
      package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa"  . 0)
                                   ("devel" . -1)))

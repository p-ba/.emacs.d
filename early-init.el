(defvar minimal-emacs--backup-gc-cons-threshold gc-cons-threshold
  "Backup of the original value of `gc-cons-threshold' before startup.")

(defvar emacs-debug nil
  "Debug flag")

(defvar emacs-native-comp t
  "Enable natve complilation")

;; Prefer loading newer compiled files
(setq load-prefer-newer t)
(setq debug-on-error emacs-debug)

(setq gc-cons-threshold most-positive-fixnum)

(defvar emacs-gc-cons-threshold (* 32 1024 1024))

(setq garbage-collection-messages emacs-debug)

;;; Native compilation and Byte compilation

(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (when emacs-native-comp
      ;; Activate `native-compile'
      (setq native-comp-deferred-compilation t
            native-comp-jit-compilation t
            package-native-compile t))
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

(setq native-comp-warning-on-missing-source emacs-debug
      native-comp-async-report-warnings-errors (or emacs-debug 'silent)
      native-comp-verbose (if emacs-debug 1 0))

(setq jka-compr-verbose emacs-debug)
(setq byte-compile-warnings emacs-debug
      byte-compile-verbose emacs-debug)

;;; Miscellaneous

(set-language-environment "UTF-8")

;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 2 1024 1024))  ; 1024kb

(setq process-adaptive-read-buffering nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

(setq warning-minimum-level (if emacs-debug :warning :error))
(setq warning-suppress-types '((lexical-binding)))

(when emacs-debug
  (setq message-log-max 16384))

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
  (unless emacs-debug
    (unless (eq system-type 'darwin)
      (setq command-line-ns-option-alist nil))
    (unless (memq initial-window-system '(x pgtk))
      (setq command-line-x-option-alist nil))))

(setq inhibit-splash-screen t)

(setq package-archives '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("devel" . "https://elpa.gnu.org/devel/"))
      package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa"  . 0)
                                   ("devel" . -1)))

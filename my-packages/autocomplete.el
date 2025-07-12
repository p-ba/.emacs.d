;;; autocomplete.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(with-eval-after-load "dabbrev"
  ;; Swap M-/ and C-M-/
  (global-set-key (kbd "M-/") 'dabbrev-completion)
  (global-set-key (kbd "C-M-/") 'dabbrev-expand)
  (global-set-key [remap dabbrev-expand] 'hippie-expand)
  (setq dabbrev-case-fold-search nil
		dabbrev-case-replace nil)
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(icomplete-vertical-mode)

(use-package icomplete
  :bind (:map icomplete-minibuffer-map
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)
              ("RET" . icomplete-force-complete-and-exit)))

(use-package cape
  :ensure t
  :init
  (defun cape-dabbrev-dict-keyword ()
    (cape-wrap-super #'cape-dabbrev #'cape-keyword))
  (add-hook 'completion-at-point-functions #'cape-dabbrev-dict-keyword)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package corfu
  :ensure t
  :custom
  ;; make the popup appear quicker
  (corfu-popupinfo-delay '(0.5 . 0.5))
  ;; always have the same width
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-scroll-margin 4)
  ;; have corfu wrap around when going up
  (corfu-cycle t)
  (corfu-preselect-first t)
  :init
  ;; enable corfu
  (global-corfu-mode t)
  ;; enable corfu history mode to act like `prescient'
  (corfu-history-mode t)
  ;; Allow Corfu to show help text next to suggested completion
  (corfu-popupinfo-mode t))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(provide 'autocomplete)

;;; autocomplete.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(setq completion-auto-help nil)
(setq ido-use-filename-at-point 'guess)
(fido-vertical-mode)

(setq complietion-styles '(basic substring flex))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
        ("C-M-/" . dabbrev-expand))
  :config
  (global-set-key [remap dabbrev-expand] 'hippie-expand)
  (setq dabbrev-case-fold-search nil
		dabbrev-case-replace nil)
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package fzf-native
  :ensure t
  :vc (:url "https://github.com/dangduc/fzf-native" :branch "main")
  :config
  (fzf-native-load-dyn)
  (setq fussy-score-fn 'fussy-fzf-native-score))

(use-package fussy
  :ensure t
  :config
  (setq fussy-score-ALL-fn 'fussy-fzf-score)
  (setq fussy-filter-fn 'fussy-filter-default)
  (setq fussy-use-cache t)
  (setq fussy-compare-same-score-fn 'fussy-histlen->strlen<)
  (fussy-setup)
  (fussy-eglot-setup))

(use-package cape
  :ensure t
  :init
  (defun cape-dabbrev-dict-keyword ()
    (cape-wrap-super #'cape-dabbrev #'cape-dict #'cape-keyword))
  (add-hook 'completion-at-point-functions #'cape-dabbrev-dict-keyword)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package completion-preview
  :ensure nil
  :bind (:map completion-preview-active-mode-map
              ("C-n" . #'completion-preview-next-candidate)
              ("C-p" . #'completion-preview-prev-candidate))
  :custom
  (completion-preview-minimum-symbol-length 1)
  :init
  (global-completion-preview-mode))

(use-package corfu
  :ensure t
  :custom
  ;; Make the popup appear quicker
  (corfu-popupinfo-delay '(0.5 . 0.5))
  ;; Always have the same width
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-scroll-margin 4)
  ;; Have Corfu wrap around when going up
  (corfu-cycle t)
  (corfu-preselect-first t)
  :init
  ;; Enable Corfu
  (global-corfu-mode t)
  ;; Enable Corfu history mode to act like `prescient'
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

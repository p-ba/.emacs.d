(unless (package-installed-p 'nano-emacs)
  (package-vc-install "https://github.com/rougier/nano-emacs"))
(require 'nano-layout)
(require 'nano-faces)
(require 'nano-theme)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(require 'nano-theme-dark)
(require 'nano-theme-light)
(nano-theme-set-dark)
(call-interactively 'nano-refresh-theme)
(require 'nano-session)
(require 'nano-modeline)
(require 'nano-bindings)
(use-package emacs
  :config
  (set-face-attribute 'default nil :font "Iosevka Comfy" :height 170))

(provide 'theme)

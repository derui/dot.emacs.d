(eval-when-compile
  (require 'use-package))

(use-package neotree
  :commands (neotree-select-up-node neotree-toggle neotree-show)
  :config
  (setq neo-keymap-style 'concise)

  :config
  (setq neo-show-hidden-files t)
  (setq neo-smart-open t)
  (setq neo-vc-integration '(face char))
  (define-key neotree-mode-map (kbd "C-l") #'neotree-select-up-node))

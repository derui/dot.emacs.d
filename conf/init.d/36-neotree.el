(eval-when-compile
  (require 'use-package))

(use-package neotree
  :commands (neotree-select-up-node neotree-toggle neotree-show)
  :bind (:map neotree-mode-map
              ("C-l" . neotree-select-up-node))
  :config
  (setq neo-keymap-style 'concise)
  (setq neo-show-hidden-files t)
  (setq neo-smart-open t)
  (setq neo-vc-integration '(face char)))

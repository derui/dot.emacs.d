(eval-when-compile
  (require 'use-package))

(use-package smartparens
  :commands (smartparens-global-mode)
  :config
  (smartparens-global-mode 1))

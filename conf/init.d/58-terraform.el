(eval-when-compile
  (require 'use-package))

(use-package terraform-mode
  :ensure t
  :mode (("\\.tf$" . terraform-mode)))

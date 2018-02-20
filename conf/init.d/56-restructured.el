(eval-when-compile
  (require 'use-package))

(use-package rst :defer t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))
    ))

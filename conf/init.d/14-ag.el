(eval-when-compile
  (require 'use-package))

(use-package ag)

(use-package wgrep-ag :defer t
  :config
  (progn
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)
    (define-key ag-mode-map (kbd "r") 'wgrep-change-to-wgrep-mode)))

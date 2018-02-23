(eval-when-compile
  (require 'use-package))

(use-package ag
  :ensure t)

(use-package wgrep-ag
  :defer t
  :ensure t
  :commands (wgrep-ag-setup)
  :bind (:map ag-mode-map
              ("r" . wgrep-change-to-wgrep-mode))
  :hook ((ag-mode-hook . wgrep-ag-setup)))

(eval-when-compile
  (require 'use-package))

(use-package ag)

(use-package wgrep-ag
  :after (ag)
  :bind (:map ag-mode-map
              ("r" . wgrep-change-to-wgrep-mode))
  :hook ((ag-mode-hook . wgrep-ag-setup)))

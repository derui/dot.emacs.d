(eval-when-compile
  (require 'use-package))

(use-package company :commands (company-mode-on))
(use-package company-go :commands (company-go))
(use-package go-eldoc :commands (go-eldoc-setup))

(use-package go-mode
  :bind (:map go-mode-map
              ("M-." . godef-jump))
  :hook ((go-mode-hook . my:go-mode-hook-0))
  :config
  (defun my:go-mode-hook-0 ()
    (setq-local tab-width 2)
    (setq-local company-backends '(company-go))

    (company-mode-on)
    (go-eldoc-setup)))

(eval-when-compile
  (require 'use-package))

(use-package company :commands (company-mode))
(use-package company-go :commands (company-go))
(use-package go-eldoc :commands (go-eldoc-setup))

(use-package go-mode
  :config
  (setq gofmt-command "goimports")

  (defun my:go-mode-hook-0 ()
    (local-set-key (kbd "M-.") 'godef-jump)
    (make-local-variable 'before-save-hook)

    (make-local-variable 'tab-width)
    (setq tab-width 2)

    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode 1)
    (go-eldoc-setup))
  (add-hook 'go-mode-hook 'my:go-mode-hook-0))

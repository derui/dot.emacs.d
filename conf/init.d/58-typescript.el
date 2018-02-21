(eval-when-compile
  (require 'use-package))

(use-package company :commands (company-mode-on))
(use-package flycheck :commands (flycheck-mode flycheck-add-next-checker))
(use-package tide
  :commands (tide-setup)
  :defines (company-tide)
  :bind (:map typescript-mode-map
              ("<C-Tab>" . company-tide))
  :config
  (flycheck-add-next-checker 'typescript-tide 'typescript-tslint 'append))

(use-package typescript-mode
  :mode ("\\.ts$" . typescript-mode)
  :config
  (defun my:typescript-mode-hook ()
    (setq typescript-indent-level 2)

    (tide-setup)
    (flycheck-mode t)
    ;; (eldoc-mode t)
    (company-mode-on))

  (add-hook 'typescript-mode-hook 'my:typescript-mode-hook))

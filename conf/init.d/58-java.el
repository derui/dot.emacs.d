(eval-when-compile
  (require 'use-package))

(use-package company-lsp)
(use-package flycheck :commands (flycheck-mode))

(use-package meghanada
  :commands (meghanada-mode)
  :config
  (defun my:java-mode-hook ()
    (meghanada-mode t)
    (flycheck-mode +1)
    (setq c-basic-offset 2)
    (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))

  (add-hook 'java-mode-hook #'my:java-mode-hook)

  (setq meghanada-java-path "java"))

(eval-when-compile
  (require 'use-package))

(use-package python
  :ensure nil
  :mode ("\\.py$" . python-mode)
  :config

  (use-package flycheck
    :ensure t
    :commands (flycheck-mode))

  (use-package elpy
    :defines (elpy-rpc-backend jedi:complete-on-dot)
    :ensure t
    :commands (elpy-mode pyvenv-activate elpy-enable))

  (pyvenv-activate my:virtualenv-path)
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")

  ;; use jedi via company-mode
  (setq jedi:complete-on-dot t)

  ;; python-mode実行時に実行するhookの設定
  (defun my:elpy-mode-hook-0 ()
    (setq-local indent-tabs-mode nil)
    (flycheck-mode))
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'my:elpy-mode-hook-0)

  (add-hook 'python-mode-hook 'elpy-mode))

(eval-when-compile
  (require 'use-package))

(use-package python
  :mode ("\\.py$" . python-mode))

(use-package flycheck
  :commands (flycheck-mode))

(use-package elpy
  :commands (elpy-enable)
  :hook (python-mode-hook . elpy-mode)
  :config
  ;; 拡張子が.pyのものについて、python-modeを割り当てる

  (pyvenv-activate my:virtualenv-path)
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")

  ;; use jedi via company-mode
  (setq jedi:complete-on-dot t)

  ;; python-mode実行時に実行するhookの設定
  (defun my:elpy-mode-hook-0 ()
    (setq indent-tabs-mode nil)
    (flycheck-mode)
    )
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'my:elpy-mode-hook-0))

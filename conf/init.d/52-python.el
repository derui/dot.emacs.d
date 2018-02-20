(eval-when-compile
  (require 'use-package))

(use-package elpy)

(use-package flycheck
  :config
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  )

;; 拡張子が.pyのものについて、python-modeを割り当てる
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(pyvenv-activate my:virtualenv-path)
(elpy-enable)
(elpy-use-ipython)
(setq elpy-rpc-backend "jedi")

;; use jedi via company-mode
(setq jedi:complete-on-dot t)

;; python-mode実行時に実行するhookの設定
(defun my:elpy-mode-hook-0 ()
  (setq indent-tabs-mode nil)
  (flycheck-mode)
  )
(add-hook 'elpy-mode-hook 'my:elpy-mode-hook-0)

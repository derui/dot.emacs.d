(eval-when-compile
  (require 'use-package))

(use-package flycheck :commands (flycheck-mode))
(use-package js2-mode
  :commands (js2-minor-mode)
  :mode
  ("\\.js" . js2-mode)
  ("\\.es6" . js2-mode)
  :config
  (defun js2-mode-hook-1 ()
    (setq-local js2-bounce-indent-p nil)
    (setq-local js2-basic-offset 2)
    (setq-local js2-include-browser-externs nil)
    (setq-local js2-mode-show-parse-errors nil)
    (setq-local js2-mode-show-strict-warnings nil)
    (setq-local js2-highlight-external-variables nil)
    (setq-local js2-include-jslint-globals nil)
    (js2-minor-mode 1)
    (flycheck-mode))

  (add-hook 'js-mode-hook 'js2-mode-hook-1))

(use-package rjsx-mode
  :commands (rjsx-mode)
  :mode
  ("components\\/.*\\.js\\'" . rjsx-mode)
  ("containers\\/.*\\.js\\'" . rjsx-mode)
  :config
  (defun my:rjsx-mode-hook-1 ()
    (flycheck-mode))
  (add-hook 'rjsx-mode 'my:rjsx-mode-hook-1))

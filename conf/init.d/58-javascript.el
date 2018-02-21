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
    (setq js2-bounce-indent-p nil)
    (setq js2-basic-offset 2)
    (setq js2-include-browser-externs nil)
    (setq js2-mode-show-parse-errors nil)
    (setq js2-mode-show-strict-warnings nil)
    (setq js2-highlight-external-variables nil)
    (setq js2-include-jslint-globals nil)
    (js2-minor-mode 1)
    (flycheck-mode))

  (add-hook 'js-mode-hook 'js2-mode-hook-1))

(eval-when-compile
  (require 'use-package))

(use-package js)
(use-package js2-mode)
(use-package flycheck)

(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6" . js2-mode))

(defun js2-mode-hook-1 ()
  (js2-minor-mode 1)
  (setq js2-bounce-indent-p nil)
  (setq js2-basic-offset 2)
  (setq js2-include-browser-externs nil)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-highlight-external-variables nil)
  (setq js2-include-jslint-globals nil)
  (flycheck-mode)
  )

(add-hook 'js-mode-hook 'js2-mode-hook-1)

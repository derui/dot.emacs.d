(require 'coffee-mode)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.cjsx$" . coffee-mode))
(defun my:coffee-mode-hook-0 ()
  (setq coffee-tab-width 2))

(add-hook 'coffee-mode-hook 'my:coffee-mode-hook-0)

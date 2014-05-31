(require 'coffee-mode)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(defun my:coffee-mode-hook-0 ()
  (make-local-variable 'tab-width)
  (set 'tab-width 2))

(add-hook 'coffee-mode-hook 'my:coffee-mode-hook-0)

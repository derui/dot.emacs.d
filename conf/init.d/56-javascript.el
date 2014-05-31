(require 'js)
(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))

(defun js2-mode-hook-1 ()
  (make-local-variable 'js2-basic-offset)
  (setq js2-basic-offset 2)
  )

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook 'js2-mode-hook-1)

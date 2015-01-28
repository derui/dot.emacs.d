(require 'js)
(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 )

(defun js2-mode-hook-1 ()
  (js2-minor-mode 1)
  )

(add-hook 'js-mode-hook 'js2-mode-hook-1)

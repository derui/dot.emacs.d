(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss" . scss-mode))

(defun my:scss-mode-hook-0 ()
  (set (make-local-variable 'css-indent-offset) 2)
  (setq scss-compile-at-save nil)
)
(add-hook 'scss-mode-hook 'my:scss-mode-hook-0)

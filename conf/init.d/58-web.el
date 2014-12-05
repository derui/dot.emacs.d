(require 'css-mode)
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss" . scss-mode))

(defun my:scss-mode-hook-0 ()
  (set (make-local-variable 'css-indent-offset) 2)
  (setq scss-compile-at-save nil)
)
(add-hook 'scss-mode-hook 'my:scss-mode-hook-0)

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html" . web-mode))
(defun my:web-mode-hook-0 ()
  (set (make-local-variable 'web-mode-markup-indent-offset) 2))

(add-hook 'web-mode-hook #'my:web-mode-hook-0)

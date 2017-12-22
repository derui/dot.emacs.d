(use-package css-mode)
(use-package scss-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.scss" . scss-mode))))

(defun my:scss-mode-hook-0 ()
  (set (make-local-variable 'css-indent-offset) 2)
  (setq scss-compile-at-save nil)
)
(add-hook 'scss-mode-hook 'my:scss-mode-hook-0)

;; yaml
(use-package yaml-mode :defer t
  :config
  (progn (add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))))

;; web-mode
(use-package web-mode :defer t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.html" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.rt" . web-mode))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (defun my:web-mode-hook-0 ()
      ))
  )

(add-hook 'web-mode-hook #'my:web-mode-hook-0)

;; stylus-mode
(use-package stylus-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode))
    (defun my:stylus-mode-hook-0 ())

    (add-hook 'stylus-mode-hook 'my:stylus-mode-hook-0)
    ))


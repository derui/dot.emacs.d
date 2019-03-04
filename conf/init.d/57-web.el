(eval-when-compile
  (require 'use-package))

(use-package company-css
  :commands (company-css))

(use-package rainbow-mode
  :commands (rainbow-mode))

(use-package css-mode
  :mode ("\\.scss" . scss-mode)
  :config
  (setq scss-compile-at-save nil)
  (defun my:scss-mode-hook-0 ()
    (setq-local css-indent-offset 2)
    (setq-local company-backends '(company-semantic
                                   company-files
                                   company-css))
    (rainbow-mode))
  (add-hook 'scss-mode-hook 'my:scss-mode-hook-0))

;; yaml
(use-package yaml-mode
  :mode ("\\.yml" . yaml-mode))

;; web-mode
(use-package web-mode
  :mode
  ("\\.html" . web-mode)
  ("\\.rt" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (defun my:web-mode-hook-0 ())

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'web-mode-hook #'my:web-mode-hook-0))

;; stylus-mode
(use-package stylus-mode
  :mode ("\\.styl$" . stylus-mode)
  :config
  (defun my:stylus-mode-hook-0 ())

  (add-hook 'stylus-mode-hook #'my:stylus-mode-hook-0))

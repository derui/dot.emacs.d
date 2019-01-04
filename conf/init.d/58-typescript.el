(eval-when-compile
  (require 'use-package))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck))

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts$" . typescript-mode)
         ("\\.tsx$" . web-mode))
  :bind (:map typescript-mode-map
              ("<C-Tab>" . company-tide))
  :config
  (use-package company :ensure t)
  (use-package flycheck :ensure t)

  (defun my:web-mode-hook-enable-jsx ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (my:typescript-mode-hook)))

  (defun my:typescript-mode-hook ()
    (setq typescript-indent-level 2)

    (tide-setup)
    (flycheck-mode +1)
    (make-local-variable 'flycheck-check-syntax-automatically)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    (tide-hl-identifier-mode +1)
    (eldoc-mode +1)
    ;; (eldoc-mode t)
    (company-mode +1))

  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-hook 'web-mode-hook #'my:web-mode-hook-enable-jsx)
  (add-hook 'typescript-mode-hook #'my:typescript-mode-hook))

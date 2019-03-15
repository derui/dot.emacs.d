(eval-when-compile
  (require 'use-package))

(use-package typescript-mode
  :after (lsp-mode web-mode)
  :mode (("\\.ts$" . typescript-mode)
         ("\\.tsx$" . web-mode))
  :hook ((web-mode . my:web-mode-hook-enable-jsx)
         (typescript-mode . my:typescript-mode-hook))
  :config
  (use-package company)
  (use-package flycheck)

  (defun my:web-mode-hook-enable-jsx ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setq-local web-mode-enable-auto-quoting nil)
      (my:typescript-mode-hook)))

  (defun my:typescript-mode-hook ()
    (setq typescript-indent-level 2)

    (flycheck-mode +1)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    (setq-local flycheck-javascript-eslint-executable "eslint_d")
    (setq-local company-backends '(company-lsp))

    ;; (prettier-js-mode)
    (add-node-modules-path)
    (company-mode +1)
    (lsp))

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode))

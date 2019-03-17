(eval-when-compile
  (require 'use-package))

(use-package typescript-mode
  :mode (("\\.ts$" . typescript-mode)
         ("\\.tsx$" . web-mode))
  :hook ((web-mode . my:web-mode-hook-enable-jsx)
         (typescript-mode . my:typescript-mode-hook))
  :custom
  (typescript-indent-level 2)
  :config
  (use-package company)
  (use-package flycheck)

  (defun my:web-mode-hook-enable-jsx ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setq-local web-mode-enable-auto-quoting nil)
      (my:typescript-mode-hook)))

  (defun my:typescript-mode-hook ()
    (add-node-modules-path)

    (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    (setq-local flycheck-javascript-eslint-executable "eslint_d")
    (setq-local prettier-js-args '("--parser" "typescript"))
    (setq-local prettier-js-command (cond
                                     ((executable-find "prettier_d") "prettier_d")
                                     (t "prettier")))
    (setq-local company-backends '(company-semantic company-files company-lsp))

    (prettier-js-mode +1)
    (company-mode +1)
    (flycheck-mode +1)
    (lsp))

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode))

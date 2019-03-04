(eval-when-compile
  (require 'use-package))

(use-package tide
  :ensure t
  :after (evil-leader typescript-mode company flycheck)
  :hook ((before-save . my:typescript-prettier))
  :config
  (defun my:typescript-prettier ()
    (when (my:minor-mode-active-p 'tide-mode)
      (prettier-js)))

  (evil-leader/set-key-for-mode 'typescript-mode
    ",l" #'tide-jump-to-definition
    ",r" #'tide-rename-symbol))

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts$" . typescript-mode)
         ("\\.tsx$" . web-mode))
  :bind (:map typescript-mode-map
              ("<C-Tab>" . company-tide))
  :hook ((web-mode . my:web-mode-hook-enable-jsx)
         (typescript-mode . my:typescript-mode-hook))
  :config
  (use-package company :ensure t)
  (use-package flycheck :ensure t)

  (defun my:web-mode-hook-enable-jsx ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setq-local web-mode-enable-auto-quoting nil)
      (my:typescript-mode-hook)))

  (defun my:typescript-mode-hook ()
    (setq typescript-indent-level 2)

    (tide-setup)
    (flycheck-mode +1)
    (make-local-variable 'flycheck-check-syntax-automatically)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    (setq tide-completion-ignore-case t)
    (tide-hl-identifier-mode +1)
    (eldoc-mode +1)

    (prettier-js-mode)
    (add-node-modules-path)
    (company-mode +1))

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode))

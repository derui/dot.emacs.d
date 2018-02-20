(eval-when-compile
  (require 'use-package))

(use-package typescript-mode)
(use-package company)
(use-package tide :defer t
  :config
  (progn
    (flycheck-add-next-checker 'typescript-tide
                                'typescript-tslint 'append)))

(autoload 'typescript-mode "TypeScript" nil t)

(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

(defun my:typescript-mode-hook ()
  (setq typescript-indent-level 2)

  (tide-setup)
  (flycheck-mode t)
  ;; (eldoc-mode t)
  (company-mode-on)
  (define-key typescript-mode-map (kbd "<C-Tab>") #'company-tide)
  )

(add-hook 'typescript-mode-hook 'my:typescript-mode-hook)

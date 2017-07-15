(require 'go-mode)
(require 'company-go)
(require 'go-eldoc)

(setq gofmt-command "goimports")

(defun my:go-mode-hook-0 ()
  (local-set-key (kbd "M-.") 'godef-jump)
  (make-local-variable 'before-save-hook)

  (make-local-variable 'tab-width)
  (setq tab-width 2)

  (go-eldoc-setup)
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode 1)
  )
(add-hook 'go-mode-hook 'my:go-mode-hook-0)

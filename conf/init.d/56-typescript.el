(require 'typescript)

(autoload 'typescript-mode "TypeScript" nil t)

(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

(require 'tss)
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")

(tss-config-default)
(defun my:typescript-mode-hook ()
  (setq typescript-indent-level 2)
  )

(add-hook 'typescript-mode-hook 'my:typescript-mode-hook)

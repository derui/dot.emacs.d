(require 'go-mode)
(require 'go-autocomplete)
(require 'go-eldoc)

(defun my:go-mode-hook-0 ()
  (local-set-key (kbd "M-.") 'godef-jump)
  (make-local-variable 'before-save-hook)

  (make-local-variable 'tab-width)
  (setq tab-width 2)

  (go-eldoc-setup)
  )
(add-hook 'go-mode-hook 'my:go-mode-hook-0)

(el-get 'sync '(go-mode))

(require 'go-mode)
(require 'go-autocomplete)
(require 'auto-complete-config)
(require 'go-flymake)

(defun my:go-mode-hook-0 ()
  (local-set-key (kbd "M-.") 'godef-jump)
  (make-local-variable 'before-save-hook)

  (auto-complete-mode 1)

  (make-local-variable 'tab-width)
  (setq tab-width 2)
  )
(add-hook 'go-mode-hook 'my:go-mode-hook-0)

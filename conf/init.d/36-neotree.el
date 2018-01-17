(use-package neotree
  :init
  (progn
    (setq neo-keymap-style 'concise))
  :config
  (progn

    (setq neo-show-hidden-files t)
    (setq neo-smart-open t)
    (setq neo-vc-integration '(face char))
    (define-key neotree-mode-map (kbd "C-l") #'neotree-select-up-node)
    ))

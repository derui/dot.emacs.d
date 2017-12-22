(use-package markdown-mode :defer t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
    ))

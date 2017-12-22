(use-package lua-mode
  :config
  (progn
    (autoload 'lua-mode "lua-mode" "Lua editing mode" t)
    (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
    ))


(require 'clojure-mode)
(eval-after-load "clojure-mode"
  (progn
    (require 'clj-refactor)
    (require 'cider)
    (require 'company)
    (require 'smartparens)

    (defun my:clojure-mode-hook-0 ()
      (smartparens-strict-mode 1)
      (clj-refactor-mode 1)
      (cljr-add-keybindings-with-prefix "C-c j"))

    (add-hook 'clojure-mode-hook #'my:clojure-mode-hook-0)

    ;; cider
    (setq cider-repl-display-in-current-window t
          cider-repl-use-clojure-font-lock t
          cider-prompt-save-file-on-load 'always-save
          cider-font-lock-dynamically '(macro core function var)
          cider-overlays-use-font-lock t)
    (cider-repl-toggle-pretty-printing)
    (defun my:cider-mode-hook-0 ()
      (eldoc-mode 1))

    (add-hook 'cider-mode-hook #'my:cider-mode-hook-0)))

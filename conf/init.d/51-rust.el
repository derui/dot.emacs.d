(eval-when-compile
  (require 'use-package))

(use-package rust-mode
  :config
  (progn
    (setq rust-indent-offset 4)
    (setq racer-rust-src-path my:rust-src-location)
    (setq racer-cmd my:rust-racer-path)

    (defun my:rust-mode-hook-0 ()
      (racer-mode 1)
      (eldoc-mode 1))

    (add-hook 'rust-mode-hook 'my:rust-mode-hook-0)

    ))

(require 'rust-mode)
(eval-after-load "rust-mode"
  '(require 'racer))

(setq rust-indent-offset 2)
(setq racer-rust-src-path my:rust-src-location)
(setq racer-cmd my:rust-racer-path)

(defun my:rust-mode-hook-0 ()
  (auto-complete-mode -1)

  (racer-mode 1)
  (eldoc-mode 1)
  )
(add-hook 'rust-mode-hook 'my:rust-mode-hook-0)

(eval-when-compile
  (require 'use-package))

(defvar my:rust-racer-path)
(defvar my:rust-src-location)

(use-package rust-mode
  :custom
  (rust-indent-offset 4)
  (racer-rust-src-path my:rust-src-location)
  (racer-cmd my:rust-racer-path)
  :hook ((rust-mode . racer-mode)
         (rust-mode . eldoc-mode)))

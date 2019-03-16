(eval-when-compile
  (require 'use-package)
  (require 'cl-lib))


;; shackleを利用する設定
(use-package shackle
  :custom
  (shackle-rules '((compilation-mode :align t :size 0.4)))
  (shackle-default-rule '(:select t))
  :hook
  ((after-init . shackle-mode)))

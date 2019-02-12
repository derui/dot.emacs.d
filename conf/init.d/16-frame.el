(eval-when-compile
  (require 'use-package)
  (require 'cl-lib))

;; shackleを利用する設定
(use-package shackle
  :ensure t
  :config
  (setq
   shackle-rules '((compilation-mode :align t :size 0.4))
   shackle-default-rule '(:select t))

  (shackle-mode 1))

(eval-when-compile
  (require 'use-package)
  (require 'cl-lib))

;; shackleを利用する設定
(use-package shackle
  :ensure t
  :config
  (winner-mode 1)

  (setq shackle-rules
        ;; ewwのbufferは右側に表示する
        '(("*eww*" :align right))))

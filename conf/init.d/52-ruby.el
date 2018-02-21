(eval-when-compile
  (require 'use-package))

;; (@* "ruby関連の設定")

(use-package ruby-mode
  :mode ("\\.rb$" . ruby-mode)
  :bind (("C-c x" . xmp)
         ("C-M-i" . rct-complete-symbol--anything)))

(use-package ruby-end
  :hook (ruby-mode . ruby-end-mode))

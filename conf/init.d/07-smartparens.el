(eval-when-compile
  (require 'use-package))

(use-package smartparens
  :ensure t
  :config
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (smartparens-global-mode 1))

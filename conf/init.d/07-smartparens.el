(eval-when-compile
  (require 'use-package))

(use-package smartparens
  :commands (sp-local-pair smartparens-global-mode)
  :config
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-mode "`" nil :actions nil)
  (smartparens-global-mode 1))

(eval-when-compile
  (require 'use-package))

(use-package auto-save-buffers-enhanced
  :ensure t
  :config
  (setq auto-save-buffers-enhanced-interval 2.0)
  (auto-save-buffers-enhanced t))

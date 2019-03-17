(eval-when-compile
  (require 'use-package))

(use-package auto-save-buffers-enhanced
  :custom
  (auto-save-buffers-enhanced-interval 3.0)
  :config 
  (auto-save-buffers-enhanced t))

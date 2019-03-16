(eval-when-compile
  (require 'use-package))

(use-package auto-save-buffers-enhanced
  :custom
  (auto-save-buffers-enhanced-interval 3.0)
  :hook ((after-init . auto-save-buffers-enhanced)))

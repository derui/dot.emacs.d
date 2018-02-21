(eval-when-compile
  (require 'use-package))

(use-package adoc-mode
  :commands (adoc-mode)
  :mode ("\\.adoc\\'" . adoc-mode))

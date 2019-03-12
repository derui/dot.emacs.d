;;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'cl-lib)
  (require 'use-package))

(use-package flycheck)
(use-package flycheck-posframe
  :ensure t
  :after (flycheck)
  :hook ((flycheck-mode . flycheck-posframe-mode)))

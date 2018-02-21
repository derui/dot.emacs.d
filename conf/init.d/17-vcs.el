(eval-when-compile
  (require 'use-package))

(use-package git-gutter
  :defer t
  :commands (global-git-gutter-mode)
  :config
  (progn
    (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))

    ;; 全体でgit-gutterを有効にする
    (global-git-gutter-mode t)))

(use-package magit :defer t)

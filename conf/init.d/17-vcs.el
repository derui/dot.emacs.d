(eval-when-compile
  (require 'use-package))

(use-package git-gutter
  :ensure t
  :config
  (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))

  ;; 全体でgit-gutterを有効にする
  (global-git-gutter-mode t))

(use-package magit
  :ensure t
  :defer t
  :hook ((git-commit-mode-hook . my:flyspell-enable)))

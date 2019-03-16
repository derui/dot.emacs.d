(eval-when-compile
  (require 'use-package))

(use-package git-gutter
  :custom
  (git-gutter:update-hooks '(after-save-hook after-revert-hook))

  ;; 全体でgit-gutterを有効にする
  :hook ((after-init . global-git-gutter-mode)))

(use-package magit
  :hook ((git-commit-mode . my:flyspell-enable)))

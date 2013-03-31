(require 'git-gutter)
;; 全体でgit-gutterを有効にする
(global-git-gutter-mode t)

;; 保存時に更新する。
(add-hook 'after-save-hook
          (lambda ()
            (if (zerop (call-process-shell-command "git rev-parse --show-toplevel"))
                (git-gutter))))

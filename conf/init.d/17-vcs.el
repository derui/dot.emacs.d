(require 'git-gutter)
;; 全体でgit-gutterを有効にする
(global-git-gutter-mode t)

(setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
(setq git-gutter:update-threshold 2)

;; 保存時に更新する。
(add-hook 'after-save-hook
          (lambda ()
            (if (zerop (call-process-shell-command "git rev-parse --show-toplevel"))
                (git-gutter))))

(require 'magit)

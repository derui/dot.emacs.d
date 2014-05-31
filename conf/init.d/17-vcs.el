(require 'git-now)

;; @の連打でgit-nowが行われるように
(key-chord-define-global "@@" 'now)

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

;; 選択している部分の背景色が、黒ベースの背景だとかなり見づらかったため、
;; 少しマイルドにする。
(require 'magit)
(set-face-background 'magit-item-highlight "DarkBlue")
(set-face-background 'magit-item-highlight "#202020")
(set-face-foreground 'magit-diff-add "#40ff40")
(set-face-foreground 'magit-diff-del "#ff4040")
(set-face-foreground 'magit-diff-file-header "#4040ff")


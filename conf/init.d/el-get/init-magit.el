;; 選択している部分の背景色が、黒ベースの背景だとかなり見づらかったため、
;; 少しマイルドにする。
(require 'magit)
(set-face-background 'magit-item-highlight "DarkBlue")
(set-face-background 'magit-item-highlight "#202020")
(set-face-foreground 'magit-diff-add "#40ff40")
(set-face-foreground 'magit-diff-del "#ff4040")
(set-face-foreground 'magit-diff-file-header "#4040ff")

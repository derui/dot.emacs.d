;; 他のelispに絡まない、基本的な関数やマクロなどを定義する。

;; 現在のバッファリスト名を取得する。
(defun my:buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))

;; ユーザーが独自に登録可能なsave-buffer-hookへのhook。
(defvar my:save-buffer-hook nil)

;; (@* "保存時に自動的にタイムスタンプを更新する")
;; 保存時に、$Lastupdate yyyy/mm/dd hh:mm:ss$という書式を発見したら、現在時刻
;; に書き換える。
(defun my:save-buffer-wrapper ()
  (interactive)
  (let ((tostr (concat "$Lastupdate: " (format-time-string "%Y/%m/%d %k:%M:%S") " $")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "\\$Lastupdate\\([0-9/: ]*\\)?\\$" nil t)
        (replace-match tostr nil t)))))
(add-hook 'my:save-buffer-hook 'my:save-buffer-wrapper)

;; hookを実行するようにする。
(add-hook 'save-buffer-hook '(lambda () (run-hooks my:save-buffer-hook)))

;; 実行したモードにおいて、常に自動でインデントを行うようにする。
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(let ((envs '("GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

;; tmuxとの連携に利用する関数などを定義する

;; (@* "tmuxに依存した各種ユーティリティ関数")

;; tmuxのパス。存在しない場合は/usr/binにあるものとする。
(defvar my:tmux-bin-path (if (executable-find "tmux") "tmux" "/usr/bin/tmux"))

(defun my:get-tmux-exec-command (cmd)
  "Return command-string of tmux"
  (format "%s %s" my:tmux-bin-path cmd))

(defun my:tmux-save-buffer (data &optional e)
  "Set data to buffer of tmux (use `tmux set-buffer')"
  (interactive "ssave to tmux buffer: ")
  (when data
    (call-process-shell-command
     (my:get-tmux-exec-command (format "set-buffer \"%s\"" data))  nil nil t)
    (when e
      (message (format "set %s to buffer of tmux" data)))))

(defun my:tmux-get-buffer ()
  "Get data from current buffer fo tmux, and set to top of kill-ring"
  (interactive "*")
  (let ((buffer (get-buffer-create " *tmux-output*")))
    (when buffer
      (call-process-shell-command (my:get-tmux-exec-command "show-buffer")
                                  nil `(,buffer t) nil)
      (save-window-excursion
        (switch-to-buffer buffer)
        ;; 余分な改行を削除する。
        (kill-ring-save (point-min) (- (point-max) 1)))
      (kill-buffer buffer))))

;; kill-ring-saveしたデータを自動的にtmuxのバッファに登録する。
(defadvice kill-ring-save (around my:tmux-save-buffer-kill-ring activate)
  ad-do-it
  (my:tmux-save-buffer (current-kill 0) nil))

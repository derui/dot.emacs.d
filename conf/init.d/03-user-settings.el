;; ユーザー独自の関数や設定を記述する。ただし、外部elispに依存しないもの
;; のみとする。

;; 一文字検索
(defvar my:last-search-char nil)
(defvar my:last-search-char-direction 'forward)

;; 一文字検索（順方向）
(defun my:search-forward-with-char (char)
  "search first charactor to forward and move to it."
  (interactive "cMove to Char: ")
  (if (eq (char-after (point)) char) (forward-char))
  (and (search-forward (char-to-string char) nil t)
       (backward-char))
  (setq my:last-search-char char
        my:last-search-char-direction 'forward))

;; 一文字検索（逆方向）
(defun my:search-backward-with-char (char)
  "seach first charactor to backward and move to it."
  (interactive "cMove backward to Char: ")
  (search-backward (char-to-string char) nil t)
  (setq my:last-search-char char
        my:last-search-char-direction 'backward))

;; (@> "*scratch*をkillできないようにする")
(defun my:make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
(add-hook 'kill-buffer-query-functions
          (function (lambda ()
                      (if (string= "*scratch*" (buffer-name))
                          (progn (my:make-scratch 0) nil)
                        t))))

;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
(add-hook 'after-save-hook
          (function (lambda ()
                      (unless (member "*scratch*" (my:buffer-name-list))
                        (my:make-scratch 1)))))

;; fluentdに対してログを送信する。
(defun my-insert-hook ()
  (start-process "post-fluent" nil "curl" "-X" "POST" "-d" (concat "json={\"mode\":\"" (symbol-name major-mode) "\"}") "localhost:9880/emacs"))
(add-hook 'post-self-insert-hook 'my-insert-hook)

;; (@> "ウィンドウ関連の設定を行う")
;; ウィンドウをインタラクティブにリサイズするための関数。
(defun my:window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-event (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

;; 分割されている場合はother-windowを、分割されていない場合は
;; 自動的にsplitを行う
(defun my:other-window ()
  (interactive)
  (if (= (length (window-list)) 1)
      (split-window))
  (other-window 1))

;; (@> "上下や左右のバッファを入れ替える")
(defun my:swap-screen()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))

(defun my:swap-screen-with-cursor()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))

;; 自動保存を行うための拡張
(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 1.0)
(auto-save-buffers-enhanced-include-only-checkout-path t)
(auto-save-buffers-enhanced t)

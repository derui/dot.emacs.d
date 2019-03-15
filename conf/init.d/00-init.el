;; 他のelispに絡まない、基本的な関数やマクロなどを定義する。
(eval-when-compile
  (require 'use-package))

(require 'cl-lib)
(defgroup my nil "My custom group" :group 'configuration)

;; 現在のバッファリスト名を取得する。
(defun my:buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))

;; ユーザーが独自に登録可能なsave-buffer-hookへのhook。
(defvar my:save-buffer-hook nil)

(add-hook 'my:save-buffer-hook #'delete-trailing-whitespace)

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
(add-hook 'my:save-buffer-hook #'my:save-buffer-wrapper)

;; hookを実行するようにする。
(add-hook 'after-save-hook #'(lambda () (run-hooks 'my:save-buffer-hook)))

;; 実行したモードにおいて、常に自動でインデントを行うようにする。
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(use-package exec-path-from-shell
  :commands (exec-path-from-shell-copy-envs)
  :config
  (let ((envs '("GOROOT" "GOPATH")))
    (exec-path-from-shell-copy-envs envs)))


(defun my:minor-mode-active-p (mode)
  "return specified minor mode is active or not"
  (let ((active-modes (cl-remove-if-not (lambda (it) (and (boundp it) (symbol-value it))) minor-mode-list)))
    (member mode active-modes)))

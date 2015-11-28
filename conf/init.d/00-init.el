;; 他のelispに絡まない、基本的な関数やマクロなどを定義する。

;; (@* "Macros")
;; 遅延ロードをまとめるためのマクロ
;; (lazyload (hoge) "hoge" (req 'huga))
(defmacro lazyload (func lib &rest body)
  `(when (locate-library ,lib)
     ,@(mapcar (lambda (f) `(autoload ',f ,lib nil t)) func)
     (eval-after-load ,lib
       '(progn
          ,@body))))

;; ライブラリが存在する場合にだけロードが行われるようにするマクロ
;; (req hoge)
(defmacro req (lib &rest body)
  `(when (locate-library ,(symbol-name lib))
     (require ',lib) ,@body))

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

;; (@* "ユーティリティ関数群")
;; 関数を実行する場合に、一時的にread-onlyおよびjaspace-modeを無効化する。
(defun temp-cancel-read-only (function &optional jaspace-off)
  "eval temporarily cancel buffer-read-only
&optional t is turn of jaspace-mode"
  (let ((read-only-p nil)
        (jaspace-mode-p nil))
    (when (and jaspace-off jaspace-mode)
      (jaspace-mode)
      (setq jaspace-mode-p t))
    (when buffer-read-only
      (toggle-read-only)
      (setq read-only-p t))
    (eval function)
    (when read-only-p
      (toggle-read-only))
    (when jaspace-mode-p
      (jaspace-mode))))

;; 実行したモードにおいて、常に自動でインデントを行うようにする。
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(let ((envs '("GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

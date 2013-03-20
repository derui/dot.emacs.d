;; はてな投稿関係の処理

;; (@* "simple-hatena-modeについての設定")
(require 'html-helper-mode)
(require 'simple-hatena-mode)
(require 'hatenahelper-mode)

;; はてダラスクリプトのパス(デフォルト値: hw.pl)
(setq simple-hatena-bin "~/bin/hw.pl")

;; はてダラデータを置くディレクトリ(デフォルト値: ~/.hatena)
(setq simple-hatena-root "~/.diary")

;; はてダラで使うデフォルトのはてなid(デフォルト値: nil)
(setq simple-hatena-default-id "derui")

;; はてなダイアリーライターのユーザエージェントオプション(デフォルト値:
;; simple-hatena-mode/vヴァージョン番号)
(setq simple-hatena-option-useragent "Hatena::Diary::Writer")

;; はてなダイアリーライターのパーマリンクに、タイムスタンプを使うかどう
;; かを指定する(デフォルト値: t)
(setq simple-hatena-use-timestamp-permalink-flag nil)

;; はてなダイアリーライターのデバッグモードオプション(デフォルト値: nil)
;; (setq simple-hatena-option-debug-flag t)

;; はてなダイアリーライターのタイムアウトオプション(デフォルト値: 30秒)
(setq simple-hatena-option-timeout 60)

;; はてなダイアリーライターのクッキーオプション(デフォルト値: t)
(setq simple-hatena-option-cookie-flag t)

;; hatedaraをcompileで利用できるようにする。
(defun my:hatena-submit ()
  "Submit hatena diary file."
  (interactive)
  (basic-save-buffer)
  (save-window-excursion (compile hatedara-script-file)))

;; articleとしてデータを設定する。
(defun my:hatena-prepend-article ()
  (interactive)
  (goto-char (point-min))
  (forward-line 1)
  (open-line 2)
  (insert (format-time-string "*%s*" (current-time))))
(define-key hatenahelper-mode-map (kbd "C-c C-i") 'my:hatena-prepend-article)

(defun my:hatena-mode-hook-0 ()
  (font-lock-fontify-buffer)
  (hatenahelper-mode 1))
(add-hook 'simple-hatena-mode-hook 'my:hatena-mode-hook-0)

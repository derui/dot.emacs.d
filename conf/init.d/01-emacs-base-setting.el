;; emacsの基本設定を定義する。
;; ここで設定するものは、
;; - emacsの標準添付ライブラリ
;; - emacs本体
;; に対する、各種変数設定や、関数の実行などを行う。

;; unable right-to-left language reordering
(setq-default bidi-display-reordering nil)
;; 言語環境は日本とする。
(set-language-environment 'Japanese)
;; キーボードから入力する文字コードはutf-8
(set-keyboard-coding-system 'utf-8)

;; RCS関連のコマンドはUTF8で処理されるように。
(setq process-coding-system-alist
      (append
       '(("rcs"      . utf-8)
         ("ci"       . utf-8)
         ("co"       . utf-8)
         ("rlog"     . utf-8)
         ("rcsdiff"  . utf-8)
         ("rcsmerge" . utf-8)
         ("ident"    . utf-8)
         ("rcsclean" . utf-8))
       process-coding-system-alist))

;; coding-systemで自動的に文字コードを決定する際の優先するコードリストを設定する。
(setq buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; narrowingを有効にする。
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; regionを大文字にする関数を有効にする
(put 'upcase-region 'disabled nil)

;; backspaceまたはdeleteキーで削除が行えるようにする。
(if (fboundp 'normal-erase-is-backspace-mode)
    (normal-erase-is-backspace-mode 0))

;; set-goal-columnを利用可能とする
(put 'set-goal-column 'disabled nil)

;; (@> "スタートアップページを表示しない")
(setq inhibit-startup-message t)

;; (@> "yes-or-noでいちいちうつのがだるいので、y-nでいけるようにする")
(fset 'yes-or-no-p 'y-or-n-p)

;; (@> "実行するブラウザを設定する")
(req browse-url
     (setq browse-url-browser-function 'browse-url-firefox)
     (setq browse-url-generic-program "firefox-bin")
     (setq browse-url-firefox-program "firefox-bin"))

;; mac限定の設定
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta))

;; (@> "font-lock-modeを有効化する")
(setq-default global-font-lock-mode nil)
(setq font-lock-support-mode 'jit-lock-mode)

;; (@> "バックアップファイルを作らない")
(setq backup-inhibited t)

;; 自動保存ファイルは作らない
(setq auto-save-default nil) 

;; (@> "タブキーの設定。インデント時には前の行と同じ場所に")
(setq indent-line-function 'indent-relative-maybe)

;; (@> "括弧が画面外にあるときには、括弧の中をハイライトする")
(show-paren-mode t)
;; (set-face-background 'show-paren-match-face "gray10")
;; (set-face-foreground 'show-paren-match-face "SkyBlue")

;; (@> "タブ幅は4文字とする")
(setq-default tab-width 4)

;; (@> "コメントのスタイル設定")
(setq comment-style 'indent)

;; (@> "選択領域をハイライト、あるいは反転させる")
(transient-mark-mode 1)

;; (@> "デフォルトでは字下げにタブは使わない")
(setq-default indent-tabs-mode nil)

;; (@> "M-:のミニバッファにおいて、tabで補完を効くように")
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;; (@> "同一の文字列をkill-ringに登録しないように")
(defadvice kill-new (before my:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

;; (@* "基本的なGUI設定")
;; スクロールバーは別にいらないので削除する。
(scroll-bar-mode -1)

;; メニューバーは別に不要なので隠す。
(menu-bar-mode -1)

;;ツールバーを隠す
(tool-bar-mode -1)

;; ポイントの位置を表示する
(column-number-mode t)

;; 端での折り返しを有効にする。
(setq truncate-lines t)

;; インデントを考慮してfillする。
(setq adaptive-fill-regexp "[ \t]*")

;; バックアップ領域を変更する
(setq backup-directory-alist
      `((".*" . ,(expand-file-name (concat user-emacs-directory "/backup")))))
(setq auto-save-file-name-transforms
       `((".*" ,(expand-file-name (concat user-emacs-directory "/backup")) t)))

;; ロックファイルは作成しない。
(setq create-lockfiles nil)

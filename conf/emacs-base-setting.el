;;; -*- lexical-binding: t -*-
;; emacsの基本設定を定義する。
;; ここで設定するものは、
;; - emacsの標準添付ライブラリ
;; - emacs本体
;; に対する、各種変数設定や、関数の実行などを行う。
(require 'leaf)

(leaf *standard-configuration
  :config
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

  ;; stop blink cursor
  (blink-cursor-mode 0)

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

  ;; mac限定の設定
  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'alt)
    (setq mac-command-modifier 'meta))

  (setq font-lock-support-mode 'jit-lock-mode)
  ;; (@> "バックアップファイルを作らない")
  (setq backup-inhibited t)

  ;; 自動保存ファイルは作らない
  (setq auto-save-default nil)

  ;; (@> "タブキーの設定。インデント時には前の行と同じ場所に")
  (setq indent-line-function 'indent-relative-maybe)
  ;; (@> "タブ幅は4文字とする")
  (setq-default tab-width 4)

  ;; (@> "コメントのスタイル設定")
  (setq comment-style 'indent)
  ;; (@> "デフォルトでは字下げにタブは使わない")
  (setq-default indent-tabs-mode nil)

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
  ;; always revert buffer immediatry
  (setq auto-revert-interval 1)

  ;; improve performance for lsp-mode
  (setq read-process-output-max (* 1024 1024))

  ;; ignore case in capf
  (setq completion-ignore-case t)
  (setq completion-styles `(basic
                            ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))

  ;; disable bell completely
  (setq ring-bell-function 'ignore)

  ;; display line number
  ;; @see https://www.grugrut.net/posts/201910202227/
  (leaf *line-number
    :if (version<= "27.0.0" emacs-version)
    :custom
    (display-line-numbers-width-start . t)
    :config
    (global-display-line-numbers-mode t))

  ;; (@> "全角空白、タブ、改行直前の空白に色をつける")
  (defface my-face-b-1 '((t (:background "gray"))) "face for full-width space" :group 'my)
  (defface my-face-b-2 '((t (:background "gray26"))) "face for tab" :group 'my)
  (defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) "" :group 'my)
  (defvar my-face-b-1 'my-face-b-1)
  (defvar my-face-b-2 'my-face-b-2)
  (defvar my-face-u-1 'my-face-u-1)

  (defun my:font-lock-mode (&rest args)
    (font-lock-add-keywords
     major-mode
     '(("\t" 0 my-face-b-2 append)
       ("　" 0 my-face-b-1 append)
       ("[ \t]+$" 0 my-face-u-1 append))))
  (advice-add 'font-lock-mode :before 'my:font-lock-mode)

  (add-to-list 'custom-theme-load-path (expand-file-name (concat user-emacs-directory "themes"))))

(leaf browse-url
  :require t
  :config
  (cond
   ((executable-find "firefox-bin")
    (progn
      (setq browse-url-browser-function 'browse-url-firefox)
      (setq browse-url-generic-program "firefox-bin")
      (setq browse-url-firefox-program "firefox-bin")))
   ((executable-find "chromium")
    (progn
      (setq browse-url-browser-function 'browse-url-chromium)
      (setq browse-url-generic-program "chromium")))))

(leaf *global-modes
  :config

  (global-font-lock-mode +1)
  ;; (@> "括弧が画面外にあるときには、括弧の中をハイライトする")
  (show-paren-mode t)

  (transient-mark-mode 1)

  ;; (@* "基本的なGUI設定")
  ;; スクロールバーは別にいらないので削除する。
  (scroll-bar-mode -1)

  ;; メニューバーは別に不要なので隠す。
  (menu-bar-mode -1)

  ;;ツールバー、初期設定の行・列番号数の表示を隠す
  (tool-bar-mode -1)
  (line-number-mode 0)
  (column-number-mode 0))

;; (@> "同一の文字列をkill-ringに登録しないように")
(leaf *advices
  :config
  (defun my:no-kill-new-duplicate (yank)
    (setq kill-ring (delete yank kill-ring)))
  (advice-add 'kill-new :before #'my:no-kill-new-duplicate))

;; (@* "emacsclientを利用して、外部から実行できるようにしておく")
(leaf server
  :require t
  :config
  (unless (server-running-p)
    (server-start)))

(leaf *key-binding
  :config
  (leaf *global
    :config
    ;; Emacs標準機能関係
    ;; suspend-frameは利用しない
    (global-set-key (kbd "C-z") nil)
    (global-set-key (kbd "C-h") #'backward-delete-char)
    (global-set-key (kbd "M-?") #'help-for-help)
    (global-set-key (kbd "M-d") #'my:kill-word-at-point)
    (global-set-key (kbd "C-m") #'newline-and-indent)
    (global-set-key (kbd "C-x /") #'dabbrev-expand)
    (global-set-key (kbd "C-x ,") #'delete-region)
    (global-set-key (kbd "M-;") #'comment-dwim)
    (global-set-key (kbd "C-x C-b") #'ibuffer)
    (global-set-key (kbd "C-_") #'redo)
    (global-set-key (kbd "M-y") #'counsel-yank-pop)

    ;; flymake関連
    (global-set-key (kbd "C-c d") #'credmp/flymake-display-err-minibuf)

    ;; マウスのホイールスクロールスピードを調節
    ;; (連続して回しているととんでもない早さになってしまう。特にLogicoolのマウス)
    (global-set-key [wheel-up] #'(lambda () "" (interactive) (scroll-down 1)))
    (global-set-key [wheel-down] #'(lambda () "" (interactive) (scroll-up 1)))
    (global-set-key [double-wheel-up] #'(lambda () "" (interactive) (scroll-down 1)))
    (global-set-key [double-wheel-down] #'(lambda () "" (interactive) (scroll-up 1)))
    (global-set-key [triple-wheel-up] #'(lambda () "" (interactive) (scroll-down 2)))
    (global-set-key [triple-wheel-down] #'(lambda () "" (interactive) (scroll-up 2)))

    ;; org-mode関係
    (global-set-key (kbd "C-c l") #'org-store-link)
    (global-set-key (kbd "C-c a") #'org-agenda)
    (global-set-key (kbd "C-c c")  #'org-capture)
    ;; ivy
    (global-set-key (kbd "C-;") #'ivy-switch-buffer))

  (leaf *map-local
    :config
    (define-key read-expression-map (kbd "TAB") #'lisp-complete-symbol)

    ;; isearch
    (define-key isearch-mode-map (kbd "C-h") #'isearch-delete-char)))

(leaf dired
  :require t
  :custom
  ;; Use dired as 2-screen filer
  (dired-dwim-target . t)
  ;; (@> "常に再帰的にディレクトリの削除/コピーを行なうようにする")
  (dired-recursive-copies . 'always)
  (dired-recursive-deletes . 'always)
  (dired-listing-switches . "-al --group-directories-first"))
(leaf wdired :require t)

(leaf uniquify
  :require t
  :custom
  (uniquify-buffer-name-style . 'forward)
  (uniquify-separator . "/")
  (uniquify-after-kill-buffer-p . t)    ; rename after killing uniquified
  (uniquify-ignore-buffers-re . "^\\*") ; don't muck with special buffers
  )

(leaf shell
  :require t
  :custom
  (explicit-shell-file-name . "/bin/bash")
  (shell-file-name . "/bin/bash")
  :config
  (setq shell-command-switch "-c")
  (setenv "EMACS" "t"))

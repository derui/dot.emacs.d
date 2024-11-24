;;; early-init.el --- Init file for my own -*- lexical-binding: t; -*-

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>

;;; Commentary:

;;: Customization:

;;; Code:

;; DO NOT EDIT THIS FILE DIRECTLY

(defconst my-saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq gc-cons-threshold most-positive-fixnum)

(defvar my:enable-profiler nil
  "起動時にProfilerを有効にするかどうか")

(when my:enable-profiler
  (add-hook 'after-init-hook (lambda ()
                               (profiler-stop)
                               (profiler-report)))
  
  (profiler-start 'cpu+mem)
  )

(setq user-emacs-directory (expand-file-name "~/.config/emacs-local"))

(defgroup my nil "My custom group" :group 'configuration)
(defcustom my:input-method 'japanese-mozc
  "input method"
  :group 'my
  :type 'symbol)

(defcustom my:trailing-whitespace-exclude-modes
  '(org-mode)
  "Do not trailing whitespace in these modes"
  :group 'my
  :type '(symbol))

(defcustom my:user-local-exec-path
  "~/.local/bin"
  "The location user-local executable path"
  :group 'my
  :type 'string)

;; SKK server(利用するのはyaskkserv2を利用する
(defcustom my:use-skkserver t
  "Use skk server or not"
  :group 'my
  :type 'boolean)

;; SKK serverをbuildする(要cargo)
(defcustom my:build-skkserver
  nil
  "Build skk server if not available in system"
  :group 'my
  :type 'boolean)

;; yaskkserv2のバージョン
(defcustom my:yaskkserv2-version "0.1.1"
  "The version of yaskkserv2"
  :group 'my
  :type 'string)

(defcustom my:use-posframe t
  "Use posframe entirely"
  :group 'my
  :type 'boolean)

(defcustom my:org-roam-db-location
  (locate-user-emacs-file "share/org-roam.db")
  "The location of database that is used by org-roam"
  :group 'my
  :type 'string)

(defcustom my:org-roam-directory
  "~/Dropbox/git/roam"
  "The location of roam files"
  :group 'my
  :type 'directory)

(defcustom my:org-roam-dailies-directory
  "~/Dropbox/git/roam-daily"
  "The location of roam-daily files"
  :group 'my
  :type 'directory)

(defcustom my:org-roam-index-file
  (expand-file-name "index.org" my:org-roam-directory)
  "The location of index file"
  :group 'my
  :type 'file)

(defcustom my:font-size 14
  "current font size"
  :group 'my
  :type 'number)

(defcustom my:font-family "Moralerspace Neon NF"
  "current font family"
  :group 'my
  :type 'string)

(defcustom my:mozc-el-locate nil
  "Location of mozc.el"
  :group 'my
  :type 'file)

(defcustom my:mozc-helper-locate nil
  "Location of emacs-mozc-helper"
  :group 'my
  :type 'file)

(defcustom my:virtualenv-path nil
  "Location of virtualenv's environment"
  :group 'my
  :type 'file)

(defcustom my:roswell-path nil
  "Location of roswell"
  :group 'my
  :type 'file)

(defcustom my:use-mozc-el nil
  "Use mozc as input method"
  :group 'my
  :type 'boolean)

(defcustom my:migemo-command nil
  "The path of migemo-like executable"
  :group 'my
  :type 'file)

(defcustom my:migemo-dictionary nil
  "The path of dictionaries for migemo"
  :group 'my
  :type 'directory)

(defcustom my:cargo-path nil
  "The path of cargo executable"
  :group 'my
  :type 'file)

(defcustom my:rust-analyzer-version nil
  "The path of rust-analyzer executable"
  :group 'my
  :type 'file)

(setq-default bidi-display-reordering nil)

(set-language-environment 'Japanese)
(set-keyboard-coding-system 'utf-8)

(setq buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(if (fboundp 'normal-erase-is-backspace-mode)
    (normal-erase-is-backspace-mode 0))

(setq inhibit-startup-screen t)

(defun my:make-untitled-buffer ()
  "untitledなbufferを用意する"
  (let ((buffer (get-buffer-create "<untitled>")))
    (with-current-buffer buffer
      (fundamental-mode))
    buffer)
  )

(setq initial-buffer-choice #'my:make-untitled-buffer)

(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-inhibited t)

(setq indent-line-function #'indent-relative-first-indent-point)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)

(setq comment-style 'indent)

(setq truncate-lines t)

(setq adaptive-fill-regexp "[ \t]*")

(setq create-lockfiles nil)

(setq read-process-output-max (* 8 1024 1024))

(setq completion-ignore-case t)
(setq completion-styles `(basic
                          ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))

(setq ring-bell-function 'ignore)

(blink-cursor-mode 0)

;; defaultのカーソルはbar
(setq-default cursor-type 'bar)

(setq require-final-newline t)

(setq vc-follow-symlinks t)

(setq use-dialog-box nil)

(setopt native-comp-async-report-warnings-errors t)

(setopt switch-to-buffer-obey-display-actions t)

(setopt split-height-threshold nil)
(setopt split-width-threshold 0)

(setopt history-delete-duplicates t)

;; process毎にバッファリングするようにする
(setq process-adaptive-read-buffering t)

;; 対応する括弧を入力したときに何もしない
(setopt blink-matching-paren nil)

;; 主にmodus themeにおいて、lisp codeの実行を許容するための設定
(setopt custom-safe-themes t)

(setq package-enable-at-startup nil)

;; scroll barを表示しない
(scroll-bar-mode -1)
;; menu barを表示しない
(menu-bar-mode -1)
;; tool barを表示しない
(tool-bar-mode -1)
;; 行番号を表示しない
(line-number-mode -1)
;; 列番号を表示しない
(column-number-mode -1)
;; 小さいサイズのwindow は拡張するだけにする
(setopt resize-mini-windows 'grow-only)

(defun my:font-setup (mode &optional family font-size)
  "Initialize fonts on window-system.

`MODE' should be either `init' or `update'. `init' affects only
initialization process. `update' affects all frames launched.
"
  (let ((emoji-font "Noto Color Emoji")
        (font-size (or font-size my:font-size))
        (font-family (or family my:font-family)))
    (cond
     ((eq mode 'init)
      (let ((font-name (format "%s-%d" font-family font-size)))
        (add-to-list 'default-frame-alist `(font . ,font-name)))
      )
     ((eq mode 'update)
      (cond
       ((or (eq window-system 'x) (eq window-system 'pgtk) (eq window-system 'ns))
        (let* ((size font-size)
               (font-set-family font-family)
               (h (round (* size 10))))
          (when (member emoji-font (font-family-list))
            (set-fontset-font t 'symbol (font-spec :family emoji-font) nil 'prepend))
          (set-face-attribute 'default nil :family font-set-family :height h)
          ))
       (t
        (message "Not have window-system")))
      ))
    ))

(my:font-setup 'init)

(defun my:font-resize (&optional font-size)
  "resize font interactively"
  (interactive "P")
  (let ((font-size (if font-size
                       (read-minibuffer "Font Size:")
                     my:font-size)))
    (my:font-setup 'update my:font-family font-size))
  )

(setq redisplay-skip-fontification-on-input t)

(provide 'early-init)

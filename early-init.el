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

(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-inhibited t)

(setq auto-save-default nil)

(setq indent-line-function #'indent-relative-first-indent-point)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)

(setq comment-style 'indent)

(setq truncate-lines t)

(setq adaptive-fill-regexp "[ \t]*")

(setq create-lockfiles nil)

(setq auto-revert-interval 1)

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

(provide 'early-init)

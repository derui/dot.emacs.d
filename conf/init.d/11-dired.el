;;; -*- lexical-binding: t -*-
;; モードラインに関係するパッケージの設定
(eval-when-compile
  (require 'cl-lib)
  (require 'use-package))

(require 'dired)
(require 'wdired)

(setq dired-listing-switches "-al --group-directories-first")

;; (@> "dired関連のキーバインド設定")
;; m でマークして T で一括変換
(define-key dired-mode-map (kbd "T") 'my:dired-do-convert-coding-system)
;; 自由にリネームを行えるようにする
(define-key dired-mode-map (kbd "e") 'wdired-change-to-wdired-mode)
;; 指定したファイルから、音声ファイルを抽出する
(define-key dired-mode-map (kbd "E") 'my:ffmpeg-extract-audio)

(defun my:dired-up-directory ()
  (interactive)
  (find-alternate-file ".."))

(define-key dired-mode-map (kbd "<backspace>") #'my:dired-up-directory)

;; (@> "dired を使って、一気にファイルの coding system (漢字) を変換する")
(defvar my:dired-default-file-coding-system nil
  "*Default coding system for converting file (s).")

(defvar my:dired-file-coding-system 'no-conversion)

;; 現在diredで選択されているファイルに対して、文字コードを変換する。
(defun my:dired-convert-coding-system ()
  (let ((file (dired-get-filename))
        (coding-system-for-write my:dired-file-coding-system)
        failure)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (write-region (point-min) (point-max) file))
      (error (setq failure err)))
    (if (not failure)
        nil
      (dired-log "convert coding system error for %s:\n%s\n" file failure)
      (dired-make-relative file))))

;; (@> "常に再帰的にディレクトリの削除/コピーを行なうようにする")
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;; dired上から指定した動画から音声のみを抽出する。
(defun my:ffmpeg-extract-audio ()
  (interactive)
  (let* ((ext (file-name-extension (dired-get-filename t)))
         (extract-ext (if (string= "flv" ext) "mp3" "aac"))
         (basename (url-file-extension (dired-get-filename) t)))
    (call-process-shell-command
     (format "/usr/bin/ffmpeg -i \"%s\" -acodec copy \"%s\""
             (dired-get-filename t) (concat basename "." extract-ext)) nil nil t))
  (message (format "extract completed %s" (url-file-extension (dired-get-filename) t)))
  )

;; Use dired as 2-screen filer
(setq dired-dwim-target t)

;; treemacs
(use-package treemacs)

(use-package treemacs-evil
  :after (treemacs))

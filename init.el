;; -*- coding: utf-8 -*-

;; 各elispの構成は以下のとおりとする。
;; init.el el-getの初期設定と、基底となるディレクトリを変更する。
;;     conf/      -- 各種設定ファイルを格納
;;       site-lisp -- el-getで管理しない(できない)ようなelispを格納する
;;       init.d/   -- 各種初期化ファイルを格納する。
;;     etc/ -- 各種elispで利用するデータなどを格納する
;;     el-get/ -- el-getでインストールしたelispを格納する
;; init.elについては、各elispで利用する基本的な変数群と、loadpathなどの環境変数の設定を行う。

;; emacs -l init.elのように起動された場合の、user-emacs-directoryの設定

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)
;; (unless package-archive-contents (package-refresh-contents))

(setq gc-cons-threshold (* 1024 1024 100))
(eval-when-compile
  (require 'cl-lib))

;; 設定ファイルの基準となるディレクトリを、init.elのあるディレクトリとする
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)

(defun my:get-recuresive-directories (file-list)
  "Get file path list recuresively."
  (let ((path-list nil))
    (unless (listp file-list)
      (setq file-list (list file-list)))
    (cl-loop for x
             in file-list
             do (when (file-directory-p x)
                  (message x)
                  (progn
                    (setq path-list (push x path-list))

                    (setq path-list
                          (append
                           (my:get-recuresive-directories
                            (cl-remove-if
                             (lambda (x) (not (file-directory-p x)))
                             (cl-remove-if
                              (lambda(y) (string-match "\\.$\\|\\.svn\\|~$\\|\\.git\\$" y))
                              (directory-files x t))))
                           path-list)))))
    path-list))


;; 起動時間の計測関係
(defconst my:time-zero (current-time))
(defvar my:time-list nil)

(defun my:time-lag-calc (lag label)
  (if (assoc label my:time-list)
      (setcdr (assoc label my:time-list)
              (- lag (cdr (assoc label my:time-list))))
    (setq my:time-list (cons (cons label lag) my:time-list))))

(defun my:time-lag (label)
  (let* ((now (current-time))
         (min (- (car now) (car my:time-zero)))
         (sec (- (car (cdr now)) (car (cdr my:time-zero))))
         (msec (/ (- (car (cdr (cdr now)))
                     (car (cdr (cdr my:time-zero))))
                  1000))
         (lag (+ (* 60000 min) (* 1000 sec) msec)))
    (my:time-lag-calc lag label)))

(defun my:time-lag-print ()
  (message (prin1-to-string
            (sort my:time-list
                  (lambda  (x y)  (> (cdr x) (cdr y)))))))

(my:time-lag "total")

(add-hook 'after-init-hook #'(lambda () (my:time-lag "total") (my:time-lag-print)) t)

(setq load-path (append load-path
                        (my:get-recuresive-directories (locate-user-emacs-file "conf/site-lisp"))))

;; 一連の初期化処理を動かす
(let* ((conf-list '("conf/user-env"
                    "conf/emacs-base-setting"
                    "conf/user-defined"
                    "conf/os-setting"
                    "conf/package-config")))
  (dolist (conf conf-list)
    (load (expand-file-name conf user-emacs-directory))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-face-highlight-read ((t (:background "gray21" :underline t))))
 '(lsp-face-highlight-write ((t (:background "gray21" :underline t))))
 '(markdown-code-face ((t (:inherit nil))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face))))
 '(markup-meta-face ((t (:stipple nil :foreground "gray30" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "unknown" :family "Monospace"))))
 '(show-paren-match ((t (:foreground nil :background nil :underline "SkyBlue" :weight bold))))
 '(symbol-overlay-default-face ((t (:background "gray21" :underline t)))))
(put 'dired-find-alternate-file 'disabled nil)

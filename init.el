;; -*- coding: utf-8 -*-
;; $Lastupdate: 2013/03/19  1:07:11 $

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
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-refresh-contents)
(package-initialize)

(require 'cl)

;; 設定ファイルの基準となるディレクトリを、init.elのあるディレクトリとする
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; 渡したパスに
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
                            (remove-if
                             (lambda (x) (not (file-directory-p x)))
                             (remove-if
                              (lambda(y) (string-match "\\.$\\|\\.svn\\|~$\\|\\.git\\$" y))
                              (directory-files x t))))
                           path-list)))))
    path-list))


;; 起動時間の計測関係
(defconst my-time-zero (current-time))
(defvar my-time-list nil)

(defun my-time-lag-calc (lag label)
  (if (assoc label my-time-list)
      (setcdr (assoc label my-time-list)
              (- lag (cdr (assoc label my-time-list))))
    (setq my-time-list (cons (cons label lag) my-time-list))))

(defun my-time-lag (label)
  (let* ((now (current-time))
         (min (- (car now) (car my-time-zero)))
         (sec (- (car (cdr now)) (car (cdr my-time-zero))))
         (msec (/ (- (car (cdr (cdr now)))
                     (car (cdr (cdr my-time-zero))))
                  1000))
         (lag (+ (* 60000 min) (* 1000 sec) msec)))
    (my-time-lag-calc lag label)))

(defun my-time-lag-print ()
  (message (prin1-to-string
            (sort my-time-list
                  (lambda  (x y)  (> (cdr x) (cdr y)))))))

(my-time-lag "total")

(add-hook 'after-init-hook
          (lambda () (my-time-lag "total")
            (my-time-lag-print)
            ;;(ad-disable-regexp 'require-time)
            (switch-to-buffer
             (get-buffer "*Messages*"))
            ) t)

;; require時に自動的に時間を計測する。
(defadvice require
    (around require-time activate)
  (my-time-lag (format "require-%s"
                       (ad-get-arg 0)))
  ad-do-it
  (my-time-lag (format "require-%s"
                       (ad-get-arg 0)))
  )

(setq load-path (append load-path
                        (my:get-recuresive-directories (locate-user-emacs-file "conf/site-lisp"))))

;; バージョンごとに別々のディレクトリを使う
(let ((versioned-dir (locate-user-emacs-file (concat "packages/" emacs-version))))
  (setq el-get-dir (expand-file-name "el-get" versioned-dir)
        package-user-dir (expand-file-name "elpa" versioned-dir)))
(add-to-list 'load-path (locate-user-emacs-file (expand-file-name "el-get" el-get-dir)))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; 自作レシピファイルを読み込む位置を設定する
(add-to-list 'el-get-recipe-path (expand-file-name "recipes" user-emacs-directory))

;; 一連の初期化処理を動かす
(let* ((conf-list '("conf/el-get.el" "conf/env-specified.el" "conf/exec-path.el" "conf/startup.el")))
  (dolist (conf conf-list)
    (load (expand-file-name conf user-emacs-directory))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 2)
 '(ac-ispell-requires 4)
 '(ag-higglight-search t)
 '(ag-reuse-buffers (quote nil))
 '(ag-reuse-window (quote nil))
 '(company-idle-delay 0.5)
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(helm-ls-git-default-sources (quote (helm-source-ls-git-buffers helm-source-ls-git)))
 '(js-indent-level 2)
 '(js2-bounce-indent-p t)
 '(org-capture-templates
   (quote
    (("N" "Idea and memos" entry
      (file+headline "~/Dropbox/git/org/memo.org" "New Idea")
      "")
     ("D" "Daily work" entry
      (file+headline "~/Dropbox/git/org/working-clocks.org" "Works")
      "** %<%Y-%m-%d>"))))
 '(package-selected-packages (quote (groovy-mode eldoc-extension caml)))
 '(uniquify-buffer-name-style nil nil (uniquify))
 '(yas-global-mode t nil (yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markup-meta-face ((t (:stipple nil :foreground "gray30" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "unknown" :family "Monospace")))))

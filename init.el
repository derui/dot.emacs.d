;; -*- coding: utf-8 -*-
;; $Lastupdate: 2013/03/19  1:07:11 $

;; 各elispの構成は以下のとおりとする。
;; init.el el-getの初期設定と、基底となるディレクトリを変更する。
;;     conf/      -- 各種設定ファイルを格納
;;       site-lisp -- el-getで管理しない(できない)ようなelispを格納する
;;       init.d/   -- 各種初期化ファイルを格納する。
;;         el-get/  -- el-getで登録したファイルについての初期化設定
;;       el-get/   -- el-get用ディレクトリ
;;       recipes/  -- 独自のrecipesを格納
;;     etc/ -- 各種elispで利用するデータなどを格納する
;;     el-get/ -- el-getでインストールしたelispを格納する
;; init.elについては、各elispで利用する基本的な変数群と、loadpathなどの環境変数の設定を行う。

;; emacs -l init.elのように起動された場合の、user-emacs-directoryの設定
(require 'cl)
;; 渡したパスに
(defun my:get-recuresive-directories (file-list)
  "Get file path list recuresively."
  (let ((path-list nil))
    (unless (listp file-list)
      (setq file-list (list file-list)))
    (loop for x
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


;; 設定ファイルの基準となるディレクトリを、init.elのあるディレクトリとする
(let* ((conf-list '("exec-path.el" "el-get.el" "startup.el")))
  (setq user-emacs-directory (file-name-directory (or load-file-name
                                                      "~/.emacs.d/init.el")))
  (setq load-path (append load-path 
                          (my:get-recuresive-directories (locate-user-emacs-file "conf/site-lisp"))))

  (progn
    (dolist (conf conf-list)

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

      ;; el-get管理しないelispを管理する

      (add-to-list 'load-path (locate-user-emacs-file "conf/site-lisp"))

      (load (concat (file-name-as-directory user-emacs-directory) "conf/" conf)))))


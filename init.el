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

(require 'org)
;; Do always overwrite init.el from generated source from init.org
(org-babel-tangle-file (expand-file-name "init.org" user-emacs-directory) "init.el")
(load "init.el")

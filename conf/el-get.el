;; el-getの初期インストールも含めた、el-get関係の設定

;; el-getへのパスを通しておく。初期起動の場合、このディレクトリは無い。
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(setq-default el-get-dir (locate-user-emacs-file "el-get")
              el-get-emacswiki-base-url
              "http://raw.github.com/emacsmirror/emacswiki.org/master/"
              )

;; el-getのインストールの際、package.elが初期化されていないとエラーになって止まるため、
;; 事前に初期化
(require 'package)
(add-to-list 'package-archives
             '("melpa" .  "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; el-getが無ければインストール
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "http://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-stable-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
;; el-getのレシピの場所を設定
(add-to-list 'el-get-recipe-path (locate-user-emacs-file "recipes"))
;; el-getの各種位置を設定
(setq el-get-dir (locate-user-emacs-file "el-get")
      el-get-verbose t
      el-get-user-package-directory (locate-user-emacs-file "conf/init.d/el-get")
      el-get-generate-autoloads nil)

;; インストールしているパッケージを初期化する。
(el-get 'sync '(helm-project
                helm
                helm-descbinds
                auto-complete
                open-junk-file
                eldoc-extension
                recentf-ext
                undo-tree
                key-chord
                key-combo
                calfw
                pos-tip
                popwin
                parenthesis
                smartrep
                powerline
                helm-c-moccur
                redo+
                color-moccur
                moccur-edit
                magit
                highlight
                linkd
                bm
                yasnippet
                dropdown-list
                grep-edit
                gtags
                highlight-cl
                info+
                zlc
                diminish
                expand-region
                multiple-cursors
                ace-jump-mode
                sunrise-commander
                git-gutter
                all-ext
                ddskk
                websocket
                auto-async-byte-compile
                ))

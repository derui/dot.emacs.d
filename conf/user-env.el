(defvar my:mozc-el-locate nil)
(defvar my:mozc-helper-locate nil)
(defvar my:rust-src-location nil)
(defvar my:rust-racer-path nil)
(defvar my:virtualenv-path nil)
(defvar my:roswell-path nil)
(defvar my:gtd-base-path nil)
(defvar my:use-company-lsp t)

;; load user-env.el if it exists.
(let ((user-env (locate-user-emacs-file "conf/user-env-specified.el")))
  (when (file-exists-p user-env)
    (load user-env)))

;; exec-pathに必要なパスを追加する。
(add-to-list 'exec-path "/home/derui/.npm/bin")
(add-to-list 'exec-path "/home/derui/.nodebrew/current/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/bin")
(add-to-list 'exec-path "/usr/sbin")
(add-to-list 'exec-path (expand-file-name "bin" my:roswell-path))

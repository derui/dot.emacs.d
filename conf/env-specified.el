(defvar my:mozc-el-locate nil)
(defvar my:mozc-helper-locate nil)
(defvar my:rust-src-location nil)
(defvar my:rust-racer-path nil)
(defvar my:virtualenv-path nil)
(defvar my:roswell-path nil)
(defvar my:gtd-base-path nil)

;; load user-env.el if it exists.
(let ((user-env (locate-user-emacs-file "conf/user-env.el")))
  (when (file-exists-p user-env)
    (load user-env)))

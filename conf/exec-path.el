;; exec-pathに必要なパスを追加する。
(require 's)
(require 'exec-path-from-shell)

(add-to-list 'exec-path "/home/derui/.npm/bin")
(add-to-list 'exec-path "/home/derui/.nodebrew/current/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/bin")
(add-to-list 'exec-path "/usr/sbin")
(mapc #'(lambda (f)
          (add-to-list 'exec-path (expand-file-name f)))
      (s-split ":" (exec-path-from-shell-getenv "PATH")))

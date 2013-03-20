;;; (@* "elisp関係の設定")
;; 保存された場合に、自動的にバイトコンパイルを行うための設定
;; from rubikitch

(el-get 'sync '(auto-async-byte-compile))

(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

(defun my:emacs-lisp-hooks ()
  (setq ac-sources
        '(ac-source-words-in-buffer ac-source-symbols))
  (auto-async-byte-compile-mode)
  (set-newline-and-indent))

(add-hook 'emacs-lisp-mode-hook 'my:emacs-lisp-hooks)

;;; (@* "elisp関係の設定")
;; 保存された場合に、自動的にバイトコンパイルを行うための設定
;; from rubikitch

(require 'auto-async-byte-compile)

(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

(defun my:emacs-lisp-hooks ()
  (add-to-list 'ac-sources 'ac-source-words-in-buffer)
  (add-to-list 'ac-sources 'ac-source-symbols)
  (setq ac-auto-start 2)
  (auto-async-byte-compile-mode)
  (set-newline-and-indent))

(add-hook 'emacs-lisp-mode-hook 'my:emacs-lisp-hooks)

(require 'eldoc-extension)
;; idle時にdelayをかけない
(setq eldoc-idle-delay 0)
;; echo areaに複数行表示を有効にする
(setq eldoc-echo-area-use-multiline-p t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
